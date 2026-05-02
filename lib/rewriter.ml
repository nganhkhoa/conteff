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

let build_flat_contract_wrapper ~loc (typ : core_type) (predicate : string) (expr : expression) =
  let pred_expr =
    Ast_builder.Default.pexp_ident ~loc { txt = Longident.parse predicate; loc }
  in

  (*
    struct
      type _ Effect.t += flat_effect_name : type -> type Effect.t
    end
   *)
  let contract_module =
    let params = [ (Ast_builder.Default.ptyp_any ~loc, (NoVariance, NoInjectivity)) ] in
    let effect_typ = Ast_builder.Default.ptyp_constr ~loc (effect_t loc) [typ] in
    let ext_cons =
      Ast_builder.Default.extension_constructor ~loc
        ~name:{ txt = flat_effect_name; loc }
        ~kind:(Pext_decl ([], Pcstr_tuple [typ], Some effect_typ))
    in
    let typext = Ast_builder.Default.type_extension ~loc ~path:(effect_t loc) ~params ~constructors:[ext_cons] ~private_:Public in
    Ast_builder.Default.pmod_structure ~loc [ Ast_builder.Default.pstr_typext ~loc typext ]
  in

  let handle = [%expr
    let __handler__ = {
      Effect.Deep.retc = (fun x -> x);
      exnc = (fun e -> raise e);

      effc = fun (type a) (type b) (eff : a Effect.t) ->
        match eff with
        | Contract.V arg ->
            (Some (fun (k : (a, b) Effect.Deep.continuation) ->
              if [%e pred_expr] arg then
                Effect.Deep.continue k arg
              else
                Effect.Deep.discontinue k (Blame pos)
            ) : ((a, b) Effect.Deep.continuation -> b) option)

        | _ -> None
    } in

    Effect.Deep.match_with Effect.perform (Contract.V [%e expr])
      __handler__
  ] in

  (*
    fun pos neg cloc ->
      let module Contract = struct
        type _ Effect.t += V
      end
      in
      let handler = ... in
      match_with Effect.perform (Contract.V expr) handler

      similar to "handle {handler} op_v(expr)"
   *)
  let inner_let_module =
    Ast_builder.Default.pexp_letmodule ~loc
      { txt = Some local_module_name; loc }
      contract_module
      handle
  in
  [%expr fun pos neg cloc -> [%e inner_let_module]]

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
    | Pexp_function (params, Some (Pconstraint rettype), Pfunction_body body) ->
        let open Tmp in

        let arg_contracts, main_return_contract =
          match contract with
          | Dependent (args, ret) | Function (args, ret) -> (args, ret)
          | Flat _ as ret -> ([], ret)
        in

        let extract_param_info p =
          let rec get_info pat =
            match pat.ppat_desc with
            | Ppat_constraint (inner_pat, typ) ->
                let rec get_name p_inner =
                  match p_inner.ppat_desc with
                  | Ppat_var { txt; _ } -> txt
                  | Ppat_constraint (p_deep, _) -> get_name p_deep
                  | _ -> "unknown_param"
                in
                (get_name inner_pat, typ)
            | _ -> Location.raise_errorf ~loc:p.pparam_loc
                     "All parameters must be explicitly type-annotated (e.g., (x : int))"
          in
          match p.pparam_desc with
          | Pparam_val (_, _, pat) -> get_info pat
          | _ -> Location.raise_errorf ~loc:p.pparam_loc "Unsupported parameter format"
        in

        let param_infos = List.map extract_param_info params in
        let param_names, param_types = List.split param_infos in
        let final_rettype = rettype in

        (* let () =
          (* 1. Print the names (easy string list) *)
          Printf.printf "\n[PPX DEBUG] Param Names: [%s]\n" (String.concat "; " param_names);

          (* 2. Convert the AST types back to readable strings and print them *)
          let types_str =
            List.map (fun typ ->
              Format.asprintf "%a" Pprintast.core_type typ
            ) param_types
            |> String.concat "; "
          in
          Printf.printf "[PPX DEBUG] Param Types: [%s]\n%!" types_str
        in *)
        let params_info =
          List.map2 (fun name c -> (name, c)) param_names arg_contracts
          |> List.combine param_types
          (* Yields: (core_type * (string * contract)) list *)
        in
        let main_arg_wrappers, expanded_args_lists =
          List.map (fun (typ, (name, c)) ->
            expand_param name [] c typ
          ) params_info
          |> List.split
        in
        let expanded_args = List.flatten expanded_args_lists in

        (* 3. Expand the main return type
           If the main function is Dependent, `C'r` gets all param_types! *)
        let is_main_dependent = match contract with Dependent _ -> true | _ -> false in
        let main_ret_deps = if is_main_dependent then param_types else [] in

        let expanded_ret = expand_param "r" main_ret_deps main_return_contract final_rettype in

        let is_main_dependent = match contract with Dependent _ -> true | _ -> false in
        let main_ret_deps = if is_main_dependent then param_types else [] in

        (* expand_param also returns a tuple here, so we unpack it! *)
        let main_ret_wrapper, expanded_ret =
          expand_param "r" main_ret_deps main_return_contract final_rettype
        in

        let all_expanded_params = expanded_args @ expanded_ret in

        (* 4. Map the padded parameters into Effect Declarations *)
        let string_typ = Ast_builder.Default.ptyp_constr ~loc { txt = Lident "string"; loc } [] in

        let combined_effs =
          List.map (fun ep ->
            {
              eff_name = ep.eff_name;

              (* The payload accurately reflects: (string * [deps] * ret_type) *)
              payload_types = string_typ :: (ep.deps @ [ep.ret_type]);

              ret_type = ep.ret_type;
            }
          ) all_expanded_params
        in

        let other_bindings =
          all_expanded_params
          |> List.filter (fun ep -> ep.eff_name <> main_ret_wrapper)
          |> List.map (generate_wrapper_binding ~loc)
        in

        (* 2. Build the custom r' binding: let r' pos neg cloc x g ... = fun () -> ... *)
        let r_binding =
          let open Ast_builder.Default in
          let pos_pat = ppat_var ~loc {txt="pos"; loc}
          and neg_pat = ppat_var ~loc {txt="neg"; loc}
          and cloc_pat = ppat_var ~loc {txt="cloc"; loc} in
          let param_pats = List.map (fun n -> ppat_var ~loc {txt=n; loc}) param_names in

          let inner_body = wrap_main_body ~loc body
                             param_names main_arg_wrappers param_types is_main_dependent in

          (* Nest params: fun pos -> fun neg -> fun cloc -> fun x -> fun g -> fun () -> ... *)
          let rhs = List.fold_right (fun pat acc -> pexp_fun ~loc Nolabel None pat acc)
                      (pos_pat :: neg_pat :: cloc_pat :: param_pats)
                      (pexp_fun ~loc Nolabel None (unit_pat ~loc) inner_body)
          in
          value_binding ~loc ~pat:(ppat_var ~loc {txt=main_ret_wrapper; loc}) ~expr:rhs
        in

        let all_bindings = r_binding :: other_bindings in

        let start_call =

          (* A. Call the gatekeeper to get the thunk: (r' pos neg cloc x g ...) *)
          let r_thunk_call =
            let args = List.map (fun n -> (Nolabel, pexp_ident ~loc {txt=Lident n; loc}))
                         ("pos" :: "neg" :: "cloc" :: param_names) in
            pexp_apply ~loc (pexp_ident ~loc {txt=Lident main_ret_wrapper; loc}) args
          in

          (* B. Build the Handler Record: { retc; exnc; effc } *)
          let handler_record =
            let retc_expr = pexp_fun ~loc Nolabel None (ppat_var ~loc {txt="x"; loc})
                              (pexp_ident ~loc {txt=Lident "x"; loc}) in

            let exnc_expr = pexp_fun ~loc Nolabel None (ppat_var ~loc {txt="e"; loc})
                              (pexp_apply ~loc (pexp_ident ~loc {txt=Lident "raise"; loc})
                                [(Nolabel, pexp_ident ~loc {txt=Lident "e"; loc})]) in

            (* Assumes contract_checking is available in scope or the local module *)
            let effc_expr = pexp_ident ~loc {txt=Lident "contract_checking"; loc} in

            pexp_record ~loc [
              ({txt = Lident "retc"; loc}, retc_expr);
              ({txt = Lident "exnc"; loc}, exnc_expr);
              ({txt = Lident "effc"; loc}, effc_expr);
            ] None
          in

          (* C. Assemble: Effect.Deep.match_with <thunk> () <handler> *)
          pexp_apply ~loc (pexp_ident ~loc {txt=Ldot(Ldot(Lident "Effect", "Deep"), "match_with"); loc}) [
            (Nolabel, r_thunk_call);   (* The function to run *)
            (Nolabel, unit_exp ~loc);  (* The argument to pass (the thunk's unit) *)
            (Nolabel, handler_record)  (* The handler record *)
          ]
        in
        let body_with_wrappers = pexp_let ~loc Recursive all_bindings start_call in
        let new_body_expr = generate_contract_module ~loc combined_effs body_with_wrappers in

        let new_pexp_desc =
          Pexp_function (params, Some (Pconstraint rettype), Pfunction_body new_body_expr)
        in

        { vb with
          pvb_expr = { vb.pvb_expr with pexp_desc = new_pexp_desc };
          pvb_attributes = remove_contract_attributes vb.pvb_attributes;
          pvb_constraint = update_constraint ~loc:vb.pvb_loc vb.pvb_constraint;
        }

    | Pexp_function (_, None, _) ->
        Location.raise_errorf ~loc:vb.pvb_loc
          "Function must be annotated with type"

    | _ ->
        let new_expr = self#expression ctx vb.pvb_expr in
        match (vb.pvb_constraint, contract) with
        | (Some (Pvc_constraint { typ; _ }), Flat predicate) ->
            { vb with
              pvb_expr = build_flat_contract_wrapper ~loc:vb.pvb_loc typ predicate new_expr;
              pvb_attributes = remove_contract_attributes vb.pvb_attributes;
              pvb_constraint = update_constraint ~loc:vb.pvb_loc vb.pvb_constraint;
            }
        | (_, Flat predicate) ->
            Location.raise_errorf ~loc:cloc
              "contract expression needs value's type annotation"
        | _ ->
            (* Utils.debug_vb_source vb; *)
            Location.raise_errorf ~loc:cloc
              "Invalid expression to convert into contract"
end
