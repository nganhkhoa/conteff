open Effect
open Ppxlib
open Ast_helper
open Ast_builder.Default

open Annotation
open Context

let flat_effect_name = "V"
let local_module_name = "Contract"

let pos_label = "pos"
let neg_label = "neg"
let cloc_label = "cloc"

let effect_t loc = { txt = Ldot (Lident "Effect", "t"); loc }
let string_t loc = Ast_builder.Default.ptyp_constr ~loc { txt = Lident "string"; loc } []

(* 3. Safely extracts a variable name from complex patterns *)
let rec extract_name_from_pat pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | Ppat_constraint (inner_pat, _) -> extract_name_from_pat inner_pat
  | Ppat_alias (inner_pat, _) -> extract_name_from_pat inner_pat
  | Ppat_construct (_, Some (_, inner_pat)) -> extract_name_from_pat inner_pat
  | Ppat_variant (_, Some inner_pat) -> extract_name_from_pat inner_pat
  | _ -> "unknown_var"

(* 4. Checks both the local constraint and the pattern for a type annotation *)
let extract_type_from_vb vb =
  match vb.pvb_constraint with
  | Some (Pvc_constraint { typ; _ }) -> Some typ
  | _ ->
      let rec search_pat p =
        match p.ppat_desc with
        | Ppat_constraint (_, typ) -> Some typ
        | Ppat_alias (inner_p, _) -> search_pat inner_p
        | Ppat_construct (_, Some (_, inner_p)) -> search_pat inner_p
        | Ppat_variant (_, Some inner_p) -> search_pat inner_p
        | Ppat_tuple ps -> List.find_map search_pat ps
        | _ -> None
      in
      search_pat vb.pvb_pat

let build_flat_contract_wrapper ~loc typ variable_name predicate_name =
  let mk_ident txt = pexp_ident ~loc { txt = Lident txt; loc } in
  let mk_var txt = ppat_var ~loc { txt; loc } in

  let tuple_arg = pexp_tuple ~loc [ mk_ident "pos"; mk_ident variable_name ] in
  let contract_call = pexp_apply ~loc (mk_ident ("Contract." ^ predicate_name)) [(Nolabel, tuple_arg)] in
  let perform_expr = pexp_apply ~loc (mk_ident "Effect.perform") [(Nolabel, contract_call)] in

  let blame_params = ["pos"; "neg"; "cloc"] in
  (* Return the raw function expression without attaching attributes here *)
  List.fold_right (fun b_name acc ->
    pexp_fun ~loc Nolabel None (mk_var b_name) acc
  ) blame_params perform_expr

let rec deep_eta_expand ~loc expr typ prefix =
  (* Helper to unroll `t1 -> t2 -> t3` into a list of arguments `[t1; t2]` and a return `t3` *)
  let rec get_args t =
    match t.ptyp_desc with
    | Ptyp_arrow (lbl, t_arg, t_ret) ->
        let args, final_ret = get_args t_ret in
        ((lbl, t_arg) :: args, final_ret)
    | _ -> ([], t)
  in

  let args, ret_typ = get_args typ in
  match args with
  | [] -> expr (* Base case: it's a flat type. No expansion needed. *)
  | _ ->
      (* 1. Build the applied arguments. If an argument is a function, recurse! *)
      let applied_args =
        List.mapi (fun i (lbl, t_arg) ->
          let arg_name = prefix ^ "_" ^ string_of_int (i + 1) in
          let arg_ident = Ast_builder.Default.pexp_ident ~loc { txt = Lident arg_name; loc } in

          (* RECURSION: Expands nested functions like g_1_1 *)
          let expanded_arg = deep_eta_expand ~loc arg_ident t_arg arg_name in

          (lbl, expanded_arg)
        ) args
      in

      (* 2. Apply the base expression to our newly expanded arguments: `g g_1` *)
      let app_expr = Ast_builder.Default.pexp_apply ~loc expr applied_args in

      (* 3. Build the parameters for the new function: `(g_1 : t1)` *)
      let params =
        List.mapi (fun i (lbl, t_arg) ->
          let arg_name = prefix ^ "_" ^ string_of_int (i + 1) in
          let inner_pat = Ast_builder.Default.ppat_var ~loc { txt = arg_name; loc } in
          let typed_pat = Ast_builder.Default.ppat_constraint ~loc inner_pat t_arg in

          { pparam_desc = Pparam_val (lbl, None, typed_pat); pparam_loc = loc }
        ) args
      in

      (* 4. Construct the function with the return constraint: `fun (...) : t2 -> ...` *)
      Ast_builder.Default.pexp_function ~loc params
        (Some (Pconstraint ret_typ))
        (Pfunction_body app_expr)

let prepend_monitor_types ~loc old_typ =
  List.fold_right (fun _ acc_typ ->
    Ast_builder.Default.ptyp_arrow ~loc Nolabel (string_t loc) acc_typ
  ) [pos_label; neg_label; cloc_label] old_typ

(* The main function to update the value_constraint option *)
let update_constraint ~loc (old_constraint : value_constraint option) : value_constraint option =
  match old_constraint with
  | Some (Pvc_constraint c) ->
      Some (Pvc_constraint { c with typ = prepend_monitor_types ~loc c.typ })

  | Some (Pvc_coercion c) ->
      Some (Pvc_coercion { c with coercion = prepend_monitor_types ~loc c.coercion })

  | None -> None

let rec extract_pattern_type (pat : pattern) : core_type option =
  match pat.ppat_desc with
  | Ppat_constraint (_inner_pat, core_typ) ->
      Some core_typ

  | Ppat_alias (inner_pat, _) ->
      extract_pattern_type inner_pat

  | _ ->
      None

let has_attribute (attr_name_to_find : string) (vb : value_binding) : bool =
  List.exists (fun attr ->
    attr.attr_name.txt = attr_name_to_find
  ) vb.pvb_attributes

let is_fixed binding =
  (* TODO: do better check, we know that contract attributes == fixed
     but we want to remove the contract attribute after rewrite
   *)
  match get_contract_payload binding.pvb_attributes with
  | Some _ -> true
  | _ -> false

let binding_name binding =
  match binding.pvb_pat.ppat_desc with
  (* let x = ... *)
  | Ppat_var { txt = name; _ } -> Some name
  (* let x : type = ... *)
  | Ppat_constraint ({ ppat_desc = Ppat_var { txt = name; _ }; _ }, _) -> Some name
  (* let _ = ... *)
  | Ppat_any -> None
  | _ -> None

let root_module_of_loc (loc : Location.t) : string =
  let fname = loc.loc_start.pos_fname in
  if fname = "" || fname = "_none_" then
    "Unknown"
  else
    fname
    |> Filename.basename
    |> Filename.remove_extension
    |> String.capitalize_ascii


let walker = object (self)
  inherit [Context.t] Ast_traverse.map_with_context as super

  method! module_binding ctx mb =
    let mod_name =
      match mb.pmb_name.txt with
      | Some name -> name
      | None -> "Anonymous"
    in
    Printf.printf "[rewriter] current module is %s\n" mod_name;
    let new_current = match ctx.current with
      | Some n -> n ^ "." ^ mod_name
      | None -> mod_name
    in
    let scope = mod_name :: ctx.scope in
    let new_ctx = { ctx with current = Some new_current; scope = scope; } in
    super#module_binding new_ctx mb

  method! structure ctx items =
    (* everything in OCaml is a module, so the file is parsed as a module
       every module has a list of structure items
       the item where we interested in is a Pstr_value which has a list of bindings
       the logic of the bindings is similar to the expression
     *)

    let (_, rewritten) = List.fold_left (fun (cctx, rewritten) item ->
      match item.pstr_desc with
      (* ignore recursive and multiple let *)
      | Pstr_value (recursive, bindings) ->

          (* if recursive we have to fetch all possible protected name
             and put it in the context, only top level (these bindings)
             contracts inside is in a different scope
           *)
          let new_monitors =
            List.filter_map (fun binding ->
              match (binding_name binding, is_fixed binding) with
              | (Some name, true) ->
                let scope_str = cctx.scope |> List.rev |> String.concat "'" in
                Some { name; scope = scope_str; typ = Scope "teehee" }
              | _ -> None
            ) bindings
          in

          let new_bindings = List.map (fun binding ->
            let monitors = match recursive with
            | Recursive -> new_monitors @ cctx.monitors
            | Nonrecursive -> cctx.monitors
            in
            self#value_binding {cctx with monitors = monitors } binding
          ) bindings
          in

          let new_item = { item with pstr_desc = Pstr_value (recursive, new_bindings) } in
          ({cctx with monitors = new_monitors @ cctx.monitors}, new_item :: rewritten)
      | _ ->
          let mapped_item = self#structure_item cctx item in
          (cctx, mapped_item :: rewritten)
    ) (ctx, []) items
    in
    List.rev rewritten

  (* don't modify the context here, ctx should be modified somewhere else *)
  method! value_binding ctx vb =
    let contract = get_contract_payload vb.pvb_attributes in
    match contract with
    | Some contract ->
        Printf.printf "%s\n" (show_contract contract);
        self#transform_contract_wrapper ~cloc:vb.pvb_loc ctx contract vb
    | _ ->
      let new_expr = self#expression ctx vb.pvb_expr in
      { vb with pvb_expr = new_expr }
      (* Location.raise_errorf ~loc:vb.pvb_loc "Contract cannot be parsed correctly" *)

  method! expression ctx expr =
    match expr.pexp_desc with
    | Pexp_let (rec_flag, bindings, body) ->
        (* OCaml AST is [binding], process by their define order
           transform the binding recursively, once a let
           binding with a contract will be processed
           the variable protected name+scope is kept in monitor list
           when the identifier is used, it will be appended with blame labels
           this works for in-module reference only
           cross-files blame assignment is done by blame assignment pass
         *)

        let (new_ctx, new_bindings) = List.fold_left (fun (cctx, new_bindings) binding ->
          (* binding body is always processed but the monitor list may not updated
             if the binding is an anonymous binding (let _ = ...)
           *)

          let var_name_maybe = binding_name binding in
          let new_scope = match var_name_maybe with
          | Some name -> name :: cctx.scope
          | None -> "_" :: cctx.scope
          in
          let transformed = self#value_binding { cctx with scope = new_scope } binding in

          (* update the monitor list if we have the variable
             ignore shadowing, everything is kept
           *)

          match var_name_maybe with
          | Some name ->
              let scope_str = cctx.scope |> List.rev |> String.concat "'" in
              let monitor = {
                name; scope = scope_str; typ = Variable;
              } in
              ({cctx with monitors = monitor :: cctx.monitors}, transformed :: new_bindings)
          | None ->
            (cctx, transformed :: new_bindings)
        ) (ctx, []) bindings in

        (* transform the body of the let expression with new context collected
           after tranforming all bindings
         *)
        let mapped_body = self#expression new_ctx body in
        { expr with pexp_desc = Pexp_let (rec_flag, List.rev new_bindings, mapped_body) }

    (* | Pexp_ident { txt = Lident var_name; loc } ->
        (* an identifier is also an expression
           with the context.scope we know the current scope
           context.monitors helps identifying if it should be appended with blame labels
         *)
        let loc = expr.pexp_loc in

        let contract_kind =
          List.find_map (fun attr ->
            match attr.attr_name.txt with
            | contract_arg_anno -> Some contract_arg_anno
            | contract_dep_anno -> Some contract_dep_anno
            | contract_ret_anno -> Some contract_ret_anno
            | _ -> None
          ) expr.pexp_attributes
        in

        begin match contract_kind with
        | Some kind ->
            let clean_attrs = remove_contract_attributes expr.pexp_attributes in
            let clean_expr = { expr with pexp_attributes = clean_attrs } in

            let mk_ident name = Ast_builder.Default.pexp_ident ~loc
              { txt = Lident name; loc } in

            let arg_a, arg_b, arg_c =
              match kind with
              | k when k = contract_arg_anno
                -> (mk_ident "neg", mk_ident "pos", mk_ident "cloc")
              | k when k = contract_dep_anno
                -> (mk_ident "neg", mk_ident "cloc", mk_ident "cloc")
              | k when k = contract_ret_anno
                -> (mk_ident "pos", mk_ident "neg", mk_ident "cloc")
              | _ ->
                Location.raise_errorf ~loc "Annotation?"
            in

            let args_list = [
              (Nolabel, arg_a);
              (Nolabel, arg_b);
              (Nolabel, arg_c);
            ] in
            Ast_builder.Default.pexp_apply ~loc clean_expr args_list

        | None ->
            let monitored = List.find_opt (fun { name; _ } -> name = var_name) ctx.monitors in
            begin match monitored with
            | Some { name; typ = Scope m; _ } ->
                let arg_a = Ast_builder.Default.estring ~loc (m ^ "." ^ var_name) in
                let arg_b = Ast_builder.Default.pexp_ident ~loc { txt = Lident "__FUNCTION__"; loc } in
                let arg_c = Ast_builder.Default.estring ~loc (m ^ "." ^ name) in

                let args_list = [
                  (Nolabel, arg_a);
                  (Nolabel, arg_b);
                  (Nolabel, arg_c);
                ] in
                Ast_builder.Default.pexp_apply ~loc expr args_list

            | Some { name; typ = Nested parent; _ } ->
                Location.raise_errorf ~loc "Unsupported nested contract"

            | _ ->
                super#expression ctx expr
            end
        end *)

    | _ -> super#expression ctx expr


  method transform_contract_wrapper ~cloc ctx contract (vb : value_binding) =
    let loc = vb.pvb_loc in

    match vb.pvb_expr.pexp_desc with
    (* for now only transform a function annotated with [@contract]
       we require type signature, because the predicate contract will be
       attached to the type signature
       we only work for body expression, for function match cases, we ignore
    *)
    | Pexp_function (params, Some (Pconstraint rettype), Pfunction_body raw_body) ->


      let module Config = struct
      (* 1. Define the generic configuration record based on your suggestion *)
      type t = {
        name : string;             (* Variable name: e.g., "x", "g", "ret" *)
        blames : string list;      (* Blame ordering: ["neg"; "pos"; "cloc"] etc. *)
        is_dep : bool;             (* Dependent copy needed? *)
        contract : contract;       (* The ADT contract *)
        typ : core_type;           (* The parsed OCaml type *)
        target_expr : expression;  (* What to wrap: mk_ident "x" or the raw body *)
      }
      end in
      let open Config in


      let arg_contracts, main_return_contract =
        match contract with
        | Dependent (args, ret) | Function (args, ret) -> (args, ret)
        | Flat _ as ret -> ([], ret)
      in
      let is_main_dependent = match contract with Dependent _ -> true | _ -> false in

      let mk_ident txt = pexp_ident ~loc { txt = Lident txt; loc } in
      let mk_var txt = ppat_var ~loc { txt; loc } in

      (* 2. The unified wrapper generator *)
      let build_wrapper (config : Config.t) acc_inner =
        let is_func_contract = match config.contract with Function _ | Dependent _ -> true | Flat _ -> false in
        let contract_core_type = contract_to_core_type ~loc config.contract in
        let contract_attr =
          { attr_name = { txt = "contract"; loc };
            attr_payload = PTyp contract_core_type;
            attr_loc = loc }
        in

        let outer_expr, outer_pat =
          if is_func_contract then begin
            let arg_types, ret_typ = unroll_arrow config.typ in
            let param_names = List.mapi (fun i _ -> config.name ^ "_p" ^ string_of_int (i + 1)) arg_types in

            let app_args = List.map (fun pname -> (Nolabel, mk_ident pname)) param_names in
            let app =
              if app_args = [] then config.target_expr
              else pexp_apply ~loc config.target_expr app_args
            in

            let func_params =
              List.map2 (fun pname ptype ->
                let param_pat = ppat_constraint ~loc (mk_var pname) ptype in
                { pparam_desc = Pparam_val (Nolabel, None, param_pat); pparam_loc = loc }
              ) param_names arg_types
            in

            (* CRITICAL FIX: Ensure the constraint is wrapped correctly for the match *)
            let func_expr =
              if func_params = [] then app
              else
                (* This matches Pexp_function (params, Some (Pconstraint ret_typ), ...) *)
                pexp_function ~loc func_params (Some (Pconstraint ret_typ)) (Pfunction_body app)
            in
            (* let g = fun ... in *)
            (func_expr, mk_var config.name)
          end else begin
            (* let (x : int) = x *)
            (config.target_expr, ppat_constraint ~loc (mk_var config.name) config.typ)
          end
        in

        let typed_inner_pat = ppat_constraint ~loc (mk_var config.name) config.typ in
        let blame_args = List.map (fun b -> (Nolabel, mk_ident b)) config.blames in
        let arg_app = pexp_apply ~loc (mk_ident config.name) blame_args in

        let inner_let =
          pexp_let ~loc Nonrecursive
            [ { pvb_pat = typed_inner_pat;
                pvb_expr = arg_app;
                pvb_attributes = [];
                pvb_loc = loc;
                pvb_constraint = None } ]
            acc_inner
        in

        let dep_let next_expr =
          if config.is_dep then
            let dep_name = config.name ^ "_dep" in
            let dep_app = pexp_apply ~loc (mk_ident config.name)
              [ (Nolabel, mk_ident "neg"); (Nolabel, mk_ident "cloc"); (Nolabel, mk_ident "cloc") ] in
            let dep_pat = ppat_constraint ~loc (mk_var dep_name) config.typ in
            pexp_let ~loc Nonrecursive
              [ { pvb_pat = dep_pat; pvb_expr = dep_app; pvb_attributes = []; pvb_loc = loc; pvb_constraint = None } ]
              next_expr
          else next_expr
        in

        (* Ensure the outer pattern also has the OCaml type constraint *)
        let typed_outer_pat =
           ppat_constraint ~loc (mk_var config.name) config.typ
        in

        pexp_let ~loc Nonrecursive
          [ { pvb_pat = typed_outer_pat;
              pvb_expr = outer_expr;
              pvb_attributes = [contract_attr];
              pvb_loc = loc;
              pvb_constraint = None } ]
          (dep_let inner_let)
      in

      (* 3. Build configurations for all variables *)
      let extract_name_and_type p =
      match p.ppat_desc with
        | Ppat_constraint (p_inner, typ) ->
            let rec get_n inner = match inner.ppat_desc with
              | Ppat_var {txt; _} -> txt
              | Ppat_constraint (i, _) -> get_n i
              | _ -> "unknown"
            in (get_n p_inner, Some typ)
        | Ppat_var {txt; _} -> (txt, None)
        | _ -> ("unknown", None)
      in

      let arg_configs =
        List.map2 (fun p c ->
          (* FIXED: Destructure as a 2-tuple (name, type_opt) *)
          let arg_name, arg_type_opt =
            match p.pparam_desc with
            | Pparam_val (_, _, pat) -> extract_name_and_type pat
            | _ -> Location.raise_errorf ~loc:p.pparam_loc "Unsupported parameter"
          in
          let arg_type = match arg_type_opt with
            | Some t -> t
            | None -> Location.raise_errorf ~loc "Missing type annotation for %s" arg_name
          in
          { name = arg_name;
            blames = ["neg"; "pos"; "cloc"];
            is_dep = is_main_dependent;
            contract = c;
            typ = arg_type;
            target_expr = mk_ident arg_name }
        ) params arg_contracts
      in
      let ret_config = {
        name = "ret";
        blames = ["pos"; "neg"; "cloc"];
        is_dep = false;
        contract = main_return_contract;
        typ = rettype;
        target_expr = raw_body (* The return wraps the original body! *)
      } in

      (* 4. Fold all configs into the unified body *)
      let all_configs = arg_configs @ [ret_config] in

      (* The base case is just `ret`, because the ret_config generates:
         let [@contract] ret = ... in let ret = ret pos neg cloc in [BASE_CASE] *)
      let fully_wrapped_body =
        List.fold_right build_wrapper all_configs (mk_ident "ret")
      in

      (* 5. Reconstruct the final function signature *)
      let blame_params = ["pos"; "neg"; "cloc"] in

      let body_with_original_args =
        List.fold_right (fun p acc ->
          match p.pparam_desc with
          | Pparam_val (label, def_expr, pat) -> pexp_fun ~loc label def_expr pat acc
          | Pparam_newtype txt -> pexp_newtype ~loc txt acc
        ) params fully_wrapped_body
      in

      let final_func_expr =
        List.fold_right (fun b_name acc ->
          pexp_fun ~loc Nolabel None (mk_var b_name) acc
        ) blame_params body_with_original_args
      in

      (* 6. Trigger recursion down the new AST *)
      let recursively_transformed_expr = self#expression ctx final_func_expr in

      { vb with pvb_expr = recursively_transformed_expr; pvb_attributes = remove_contract_attributes vb.pvb_attributes }

    | Pexp_function (_, None, _) ->
        Location.raise_errorf ~loc:vb.pvb_loc
          "Function must be annotated with type"

    | _ ->
        let new_expr = self#expression ctx vb.pvb_expr in

        let vb_typ = extract_type_from_vb vb in
        match (vb_typ, contract) with
        | (Some typ, Flat predicate) ->
          let var_name = extract_name_from_pat vb.pvb_pat in

          (* Generates the clean expression: fun pos neg cloc -> Effect.perform ... *)
          let perform_expr = build_flat_contract_wrapper ~loc typ var_name predicate in

          let recursively_transformed_expr = self#expression ctx perform_expr in

          (* Build the [@do_contract : predicate] attribute here *)
          let do_contract_attr =
            let mk_ident txt = pexp_ident ~loc { txt = Lident txt; loc } in
            { attr_name = { txt = "do_contract"; loc };
              attr_payload = PStr [ { pstr_desc = Pstr_eval (mk_ident predicate, []); pstr_loc = loc } ];
              attr_loc = loc }
          in

          { vb with
            pvb_expr = recursively_transformed_expr;
            (* Prepend the new attribute while stripping the old [@contract] attribute *)
            pvb_attributes = do_contract_attr :: (remove_contract_attributes vb.pvb_attributes);
            pvb_constraint = None;
          }

        | (None, Flat _) ->
            Location.raise_errorf ~loc:cloc
              "contract expression needs value's type annotation (checked pattern and binding)"
        | _ ->
            (* Utils.debug_vb_source vb; *)
            Location.raise_errorf ~loc:cloc
              "Invalid expression to convert into contract"
end
