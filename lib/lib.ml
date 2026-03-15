open Effect
open Ppxlib
open Ast_helper
open Ast_builder.Default

open Annotation
open Mytypes

let contract_attr =
  Attribute.declare "contract"
    Attribute.Context.value_binding
    Ast_pattern.(pstr nil)
    ()

(* Define the [@@pred] attribute for types *)
let pred_attr =
  Attribute.declare "pred"
    Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let effect_t_path ~loc =
  let lid = Ldot ((Lident "Effect"), "t") in
  Located.mk ~loc lid

let rec collect_args expr acc =
  match expr.pexp_desc with
  | Pexp_function (params, _, body) ->
      (* params is a list of 'function_param' *)
      let actual_body = match body with
        | Pfunction_body e -> e
        | Pfunction_cases (cs, _, _) -> pexp_function_cases ~loc:expr.pexp_loc cs
      in
      (* We continue recursing in case there are nested functions,
         but usually, all arguments are in 'params' now. *)
      (params, actual_body)
  | _ -> ([], expr)

let build_flat_contract_wrapper ~loc (typ : core_type) (contract_name : string) (expr : expression) =

  (* 1. Convert the Flat string into the predicate function identifier (e.g., `is_pos`) *)
  let pred_expr =
    Ast_builder.Default.pexp_ident ~loc { txt = Longident.parse contract_name; loc }
  in

  (* 2. Build the local Contract module:
        struct type _ Effect.t += v : typ -> typ Effect.t end *)
  let contract_module =
    let path = { txt = Ldot (Lident "Effect", "t"); loc } in
    let params = [ (Ast_builder.Default.ptyp_any ~loc, (NoVariance, NoInjectivity)) ] in
    let effect_t = Ast_builder.Default.ptyp_constr ~loc path [typ] in
    let ext_cons =
      Ast_builder.Default.extension_constructor ~loc
        ~name:{ txt = "V"; loc }
        ~kind:(Pext_decl ([], Pcstr_tuple [typ], Some effect_t))
    in
    let typext = Ast_builder.Default.type_extension ~loc ~path ~params ~constructors:[ext_cons] ~private_:Public in
    Ast_builder.Default.pmod_structure ~loc [ Ast_builder.Default.pstr_typext ~loc typext ]
  in

  (* 3. Build the handler and match_with block *)
  let match_with_expr = [%expr
    let __handler__ = {
      Effect.Deep.retc = (fun x -> x);
      exnc = (fun e -> raise e);

      effc = fun (type a) (type b) (eff : a Effect.t) ->
        match eff with
        | Contract.V x ->
            (Some (fun (k : (a, b) Effect.Deep.continuation) ->
              if [%e pred_expr] x then
                Effect.Deep.continue k x
              else
                Effect.Deep.discontinue k (Blame pos)
            ) : ((a, b) Effect.Deep.continuation -> b) option)

        | _ -> None
    } in

    Effect.Deep.match_with Effect.perform (Contract.V [%e expr])
      __handler__
  ] in

  (* 4. Wrap the handler in the module binding *)
  let inner_let_module =
    Ast_builder.Default.pexp_letmodule ~loc
      { txt = Some "Contract"; loc }
      contract_module
      match_with_expr
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

let print_expr_ast (expr) =
  let native_expr = Selected_ast.To_ocaml.copy_expression expr in
  Format.eprintf "--- EXPRESSION AST ---@.%a@."
    (Ocaml_common.Printast.expression 0) native_expr

let print_vb_ast (vb : value_binding) =
  (* Convert ppxlib's AST into the compiler's native Parsetree *)
  let native_pat = Selected_ast.To_ocaml.copy_pattern vb.pvb_pat in
  let native_expr = Selected_ast.To_ocaml.copy_expression vb.pvb_expr in

  (* Now pass the native nodes to Printast *)
  Format.eprintf "--- PATTERN AST ---@.%a@."
    (Ocaml_common.Printast.pattern 0) native_pat;

  Format.eprintf "--- EXPRESSION AST ---@.%a@."
    (Ocaml_common.Printast.expression 0) native_expr

let prepend_monitor_types ~loc old_typ =
  let string_typ =
    Ast_builder.Default.ptyp_constr ~loc { txt = Lident "string"; loc } []
  in
  List.fold_right (fun _ acc_typ ->
    Ast_builder.Default.ptyp_arrow ~loc Nolabel string_typ acc_typ
  ) ["pos"; "neg"; "cloc"] old_typ

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
  | Ppat_var { txt = name; _ } -> Some name
  | Ppat_constraint ({ ppat_desc = Ppat_var { txt = name; _ }; _ }, _) -> Some name
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

type monitor_type =
  | Scope of string
  | Argument
  | DependentArgument
  | ReturnValue
  | Nested of string
  [@@deriving show]

type monitor =
  { name : string;
    typ  : monitor_type;
  }
  [@@deriving show]

type rewrite_context =
  { monitors : monitor list; (* keep a scope of monitored variables *)
    current : string option; (* the current scope module/function *)
  }
  [@@deriving show]

let empty_context = { monitors = []; current = None; }


let expander = object (self)
  inherit [rewrite_context] Ast_traverse.map_with_context as super

  method! module_binding ctx mb =
    let mod_name =
      match mb.pmb_name.txt with
      | Some name -> name
      | None -> "Anonymous"
    in
    let new_current = match ctx.current with
      | Some n -> n ^ "." ^ mod_name
      | None -> mod_name
    in
    let new_ctx = { ctx with current = Some new_current; } in
    super#module_binding new_ctx mb

  method! structure ctx items =
    (* this is the outermost items in the file
       we capture variables declared, and then going inside their expresion

       although unlikely users can declare same variable many times
       we should be aware of cases

          let [@contract] x = 1
          let x = 2
          let use_x = x

       here the x referenced in use_x will not be fixed into a function
     *)
    let (_, rewritten) = List.fold_left (fun (cctx, rewritten) item ->
      match item.pstr_desc with
      | Pstr_value (recursive, [binding]) when Option.is_some (binding_name binding) ->
          (* this is the outermost binding *)
          let var_name = Option.get (binding_name binding) in

          let module_name = match cctx.current with
            | Some n -> n
            | None -> root_module_of_loc binding.pvb_loc
          in
          let cctx = {cctx with current = Some module_name} in

          (* if recursive, then have to remember the name *)
          let new_binding = self#value_binding cctx binding in
          let new_item = {item with pstr_desc = Pstr_value (recursive, [new_binding])} in
          let new_monitor = Option.bind (binding_name new_binding) (fun name ->
            if is_fixed binding
            then Some { name = name; typ = Scope module_name; }
            else None
          )
          in

          (* remove the name from the monitor, it will be added again,
             if this variable has been fixed
           *)
          let new_monitors = List.filter (fun ({name; _}) -> name <> var_name) cctx.monitors in
          let new_monitors = match new_monitor with
            | Some m -> m :: new_monitors
            | None -> new_monitors
          in
          let new_ctx = {cctx with monitors = new_monitors} in
          (new_ctx, new_item :: rewritten)
      | _ ->
          let mapped_item = self#structure_item cctx item in
          (cctx, mapped_item :: rewritten)
    ) (ctx, []) items
    in
    List.rev rewritten

  method transform_contract_wrapper ctx contract (vb : value_binding) =
    let loc = vb.pvb_loc in

    match vb.pvb_expr.pexp_desc with
    (* for now only transform a function annotated with [@contract]
       we require type signature, because the predicate contract will be
       attached to the type signature
       we only work for body expression, for function match cases, we ignore
    *)
    | Pexp_function (params, Some (Pconstraint rettype), Pfunction_body body) ->
        (* for each params we rewrite into below, using shadowing to bind old value
           into their contract version
           let [@contract : param_contract] p = p in

           body is wrapped into a contract as well
           let [@contract : param_contract] __ret__ = body in
           __ret__

           labels are added when handling the expression
         *)
        let rec extract_name pat =
          match pat.ppat_desc with
          | Ppat_var { txt; _ } -> Some txt
          | Ppat_constraint (p, _) -> extract_name p
          | Ppat_alias (_, { txt; _ }) -> Some txt
          | _ -> None
        in
        let rec extract_name_and_typ pat =
          match pat.ppat_desc with
          (* We found a type constraint! Extract the type and keep digging for the name. *)
          | Ppat_constraint (inner_pat, core_typ) ->
              (match extract_name_and_typ inner_pat with
               | Some (name, _) -> Some (name, Some core_typ)
               | None -> None)
          (* Base case: we found the variable name, but no type constraint here. *)
          | Ppat_var { txt; _ } -> Some (txt, None)
          | Ppat_alias (p, { txt; _ }) -> Some (txt, None)
          | _ -> None
        in
        (* Recursively build the body by walking the contract and params together *)
        let rec build_body current_contract current_params acc_body =
          match current_contract, current_params with

          (* Case 1: We have a function contract and a parameter to match it *)
          | Function (dom, rng), param :: rest_params ->
              let inner_body = build_body rng rest_params acc_body in
              let pat = match param.pparam_desc with
                | Pparam_val (_label, _default, p) -> p
                | Pparam_newtype _ -> failwith "Newtype not supported in contract params"
              in

              (match extract_name_and_typ pat with
               | Some (p1_name, Some ast_typ) ->
                   (* SUCCESS: The parameter has a type in the AST. Use it! *)
                   let p_pat = Ast_builder.Default.ppat_var ~loc { txt = p1_name; loc } in
                   let p_pat = Ast_builder.Default.ppat_constraint ~loc p_pat ast_typ in
                   let base_ident = Ast_builder.Default.pexp_ident ~loc { txt = Lident p1_name; loc } in
                   let p_expr = deep_eta_expand ~loc base_ident ast_typ p1_name in

                   let attr = {
                     attr_name = { txt = "contract"; loc };
                     attr_payload = PTyp (core_type_of_contract ~loc dom);
                     attr_loc = loc;
                   } in
                   let arg_attr = {
                     attr_name = { txt = "contract_arg"; loc };
                     attr_payload = PStr [];
                     attr_loc = loc;
                   } in

                   let vb_with_attr = {
                     (Ast_builder.Default.value_binding ~loc ~pat:p_pat ~expr:p_expr)
                     with pvb_attributes = [attr; arg_attr]
                   } in
                   let vb_clean = Ast_builder.Default.value_binding ~loc ~pat:p_pat ~expr:base_ident in

                   Ast_builder.Default.pexp_let ~loc Nonrecursive [vb_with_attr]
                     (Ast_builder.Default.pexp_let ~loc Nonrecursive [vb_clean] inner_body)

               | _ -> inner_body) (* Fallback if parameter name is wild/unextractable *)

          (* Case 2: Base case. No more parameters, wrap the return value with the remaining rng *)
          | final_contract, [] ->
              let ret_pat = Ast_builder.Default.ppat_var ~loc { txt = "__ret__"; loc } in
              let ret_pat = Ast_builder.Default.ppat_constraint ~loc ret_pat rettype in
              (* let ret_expr = Ast_builder.Default.pexp_ident ~loc { txt = Lident "__ret__"; loc } in *)

              let base_ident = Ast_builder.Default.pexp_ident ~loc { txt = Lident "__ret__"; loc } in
              let ret_expr = deep_eta_expand ~loc base_ident rettype "__ret__" in

              let attr = {
                attr_name = { txt = "contract"; loc };
                attr_payload = PTyp (core_type_of_contract ~loc final_contract);
                attr_loc = loc;
              } in
              let ret_attr = {
                attr_name = { txt = "contract_ret"; loc };
                attr_payload = PStr [];
                attr_loc = loc;
              } in

              let ret_vb = {
                (Ast_builder.Default.value_binding ~loc ~pat:ret_pat ~expr:acc_body)
                with pvb_attributes = [attr; ret_attr]
              } in

              (* let [@contract: rng] __ret__ = body in __ret__ *)
              Ast_builder.Default.pexp_let ~loc Nonrecursive [ret_vb] base_ident

          (* Edge case: Arity mismatch between contract and parameters *)
          | _, _ -> acc_body
        in

        (* 1. Build the newly wrapped body *)
        let wrapped_body = build_body contract params body in
        let wrapped_body = self#expression ctx wrapped_body in

        (* 2. Generate the new `pos`, `neg`, `cloc` parameters *)
        let mk_extra_param name =
          let pat = Ast_builder.Default.ppat_var ~loc { txt = name; loc } in
          (* Using OCaml 5.2+ Pparam_val. If you are on 4.14, this is just (Nolabel, None, pat) *)
          { pparam_desc = Pparam_val (Nolabel, None, pat); pparam_loc = loc }
        in
        let extra_params = [mk_extra_param "pos"; mk_extra_param "neg"; mk_extra_param "cloc"] in

        (* 3. Combine them: pos, neg, cloc come first, then the original params *)
        let new_params = extra_params @ params in

        (* 4. Return the rebuilt function expression *)
        let new_body = Ast_builder.Default.pexp_function ~loc new_params None (Pfunction_body wrapped_body)
        in

        { vb with
          pvb_expr = new_body;
          pvb_attributes = [];
          pvb_constraint = update_constraint ~loc:vb.pvb_loc vb.pvb_constraint;
        }

    | _ ->
        (* this is for everything else, in ocaml this is just an expression
           we first fix all identifier correctly inside this expression
           and then wrap it up
         *)
        (* wrap the new_expr into

           let module Contract = struct
             type _ Effect.t += e : type_of_var -> type_of_var Effect.t
           end in

           let handler =
           {
             effc = fun (type e) (eff : e Effect.t) ->
               match eff with
               | Contract.e x -> Some (fun k ->
                   let valid = bigger_than_40 x in
                   if valid
                   then continue k x
                   else discontinue k (Blame pos)
                 )
               | _ -> None
           }
           in
           try_with Effect.perform (Contract.e new_expr) handler
         *)

        (* then make/extend the vb into a function with arguments pos neg cloc
         *)

        let new_expr = self#expression ctx vb.pvb_expr in
        match (vb.pvb_constraint, contract) with
        | (Some (Pvc_constraint { typ; _ }), Flat predicate) ->
            { vb with
              pvb_expr = build_flat_contract_wrapper ~loc:vb.pvb_loc typ predicate new_expr;
              pvb_attributes = [];
              pvb_constraint = update_constraint ~loc:vb.pvb_loc vb.pvb_constraint;
            }
        | _ ->
            { vb with
              pvb_expr = new_expr;
              pvb_attributes = [];
            }

  (* don't modify the context here, ctx should be modified somewhere else *)
  method! value_binding ctx vb =
    let contract = get_contract_payload vb.pvb_attributes in
    match contract with
    | Some contract ->
        (* we don't filter out the same name here, shadow valid after this binding *)
        (* let var_name = Option.get (binding_name vb) in *)
        (* Printf.eprintf "%s with contract %s (%s) current context is %s\n"
          var_name (show_contract contract) (Option.get ctx.current) (show_rewrite_context ctx); *)
        (* print_vb_ast binding; *)

        let new_expr = self#expression ctx vb.pvb_expr in
        self#transform_contract_wrapper ctx contract { vb with pvb_expr = new_expr }

    | None ->
        let new_expr = self#expression ctx vb.pvb_expr in
        { vb with pvb_expr = new_expr }

  method! expression ctx expr =
    match expr.pexp_desc with
    | Pexp_let (rec_flag, bindings, body) ->
        (* print_expr_ast expr; *)
        let (new_ctx, new_bindings) = List.fold_left (fun (cctx, new_bindings) binding ->
          (* print_vb_ast binding; *)
          let new_binding = self#value_binding cctx binding in
          let var_name = (binding_name new_binding) in
          let new_monitor = Option.bind var_name (fun name ->
            if is_fixed binding
            then (
              if has_attribute "contract_arg" binding
              then Some { name; typ = Argument }
              else if has_attribute "contract_ret" binding
              then Some { name; typ = ReturnValue }
              else Some { name; typ = Nested (Option.value ~default:"" cctx.current) })
            else None
          )
          in

          let new_monitors = List.filter (fun ({name; _}) -> name <> (Option.get var_name)) cctx.monitors in
          let new_monitors = match new_monitor with
          | Some m -> m :: new_monitors
          | None -> new_monitors
          in
          (* remove duplicated name because shadowing *)
          ({cctx with monitors = new_monitors}, new_binding :: new_bindings)
        ) (ctx, []) bindings in
        let mapped_body = self#expression new_ctx body in
        { expr with pexp_desc = Pexp_let (rec_flag, List.rev new_bindings, mapped_body) }

    | Pexp_ident { txt = Lident var_name; loc } ->
        let monitored = List.find_opt (fun { name; _; } -> name = var_name) ctx.monitors in
        begin
        match monitored with
        | Some { name; typ = Scope m; } ->
            let loc = expr.pexp_loc in
            let arg_a = Ast_builder.Default.estring ~loc m in
            let arg_b = Ast_builder.Default.pexp_ident ~loc { txt = Lident "__FUNCTION__"; loc } in
            let arg_c = Ast_builder.Default.estring ~loc (m ^ "." ^ name) in

            let args_list = [
              (Nolabel, arg_a);
              (Nolabel, arg_b);
              (Nolabel, arg_c);
            ] in
            Ast_builder.Default.pexp_apply ~loc expr args_list

        | Some { name; typ = Argument; } ->
            let loc = expr.pexp_loc in
            let arg_a = Ast_builder.Default.pexp_ident ~loc { txt = Lident "neg"; loc } in
            let arg_b = Ast_builder.Default.pexp_ident ~loc { txt = Lident "pos"; loc } in
            let arg_c = Ast_builder.Default.pexp_ident ~loc { txt = Lident "cloc"; loc } in

            let args_list = [
              (Nolabel, arg_a);
              (Nolabel, arg_b);
              (Nolabel, arg_c);
            ] in
            Ast_builder.Default.pexp_apply ~loc expr args_list

        | Some { name; typ = ReturnValue; } ->
            let loc = expr.pexp_loc in
            let arg_a = Ast_builder.Default.pexp_ident ~loc { txt = Lident "pos"; loc } in
            let arg_b = Ast_builder.Default.pexp_ident ~loc { txt = Lident "neg"; loc } in
            let arg_c = Ast_builder.Default.pexp_ident ~loc { txt = Lident "cloc"; loc } in

            let args_list = [
              (Nolabel, arg_a);
              (Nolabel, arg_b);
              (Nolabel, arg_c);
            ] in
            Ast_builder.Default.pexp_apply ~loc expr args_list

        | _ ->
            super#expression ctx expr
        end

    | _ -> super#expression ctx expr
end

let () =
  Driver.register_transformation
    ~impl:(expander#structure empty_context)
    "conteff"
