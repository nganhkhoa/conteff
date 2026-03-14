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

let build_contract_module ~loc (params : (string * core_type) list) (rettype : core_type) =
  (* build all effect types for use as the contract effect *)

  let to_effects = ("_ret", rettype) :: params in

  let effects =
    List.map (fun (name, ct) ->
      let effect_name = Located.mk ~loc (Printf.sprintf "ContractCheck_%s" name) in

      let unit_node = ptyp_constr ~loc (Located.mk ~loc (Lident "unit")) [] in
      let effect_t = ptyp_constr ~loc (effect_t_path ~loc) [unit_node] in

      let path = effect_t_path ~loc in
      let params = [ (ptyp_any ~loc, (NoVariance, NoInjectivity)) ] in
      let constructors = [
        extension_constructor ~loc
          ~name:effect_name
          ~kind:(Pext_decl ([], Pcstr_tuple [ct], Some effect_t))
      ]
      in

      pstr_typext ~loc
        (type_extension ~loc ~path ~params ~constructors ~private_:Public)
    )
    to_effects
  in

  pmod_structure ~loc effects

let build_function_wrappers ~loc params body =
  (* for any params f that is a function builds

     form of f (t0 [@pre : predicate0] -> ... -> tR [@post : predicateR])

     let [@contract] f (__f_p0 : [@pre : predicate0]) ... __f_pR : tR [@post : predicateR] =
       f __f_p0 ....
     in
     (* flip blame assignment
        inputs are blamed negative (2nd argument)
        output is  blamed positive (1st argument)

        parent function says, I will always call f with correct argument
        so if inputs are wrong, it's the fault of caller
        if f is provided with correct argument, it should output correctly
        else, who provided f is at fault
      *)
     let f = f pos neg in
   *)

  List.fold_left (fun acc_body (name, ct) ->
    let lid_loc = Located.mk ~loc (Lident name) in
    let str_loc = Located.mk ~loc name in

    let (args_types, ret_type) = flatten_arrow_type ct in
    let p_names = List.mapi (fun i _ -> Printf.sprintf "__%s__p%d" name i) args_types in

    (* f __p0 __p1 ... *)
    let apply_f = Exp.apply ~loc (Exp.ident ~loc lid_loc)
      (List.map (fun p -> (Nolabel, Exp.ident ~loc (Located.mk ~loc (Lident p)))) p_names) in

    let shadow_f_vb =
      (* 1. Build the params as you did before *)
      let params = List.map2 (fun p_name p_type ->
        let pat = Pat.constraint_ ~loc (Pat.var ~loc (Located.mk ~loc p_name)) p_type in
        { pparam_desc = Pparam_val (Nolabel, None, pat); pparam_loc = loc }
      ) p_names args_types in

      (* 2. Manually construct the Pexp_function descriptor *)
      (* This matches your recursive pattern exactly *)
      let fn_desc = Pexp_function (
        params,
        Some (Pconstraint ret_type),
        Pfunction_body apply_f
      ) in

      (* 3. Use Exp.mk to create the expression node *)
      let fn_expr = Exp.mk ~loc fn_desc in
      let attr = {
        attr_name = Located.mk ~loc "contract";
        attr_payload = PStr []; (* Empty payload for [@contract] *)
        attr_loc = loc;
      } in

      { (Vb.mk ~loc (Pat.var ~loc str_loc) fn_expr) with pvb_attributes = [attr] }
    in

    (* let f = f neg pos *)
    let blame_f_vb =
      let apply_blame = Exp.apply ~loc (Exp.ident ~loc lid_loc) [
        (* put current pos neg as argument, to prevent shadowing *)
        (Nolabel, Exp.ident ~loc (Located.mk ~loc (Lident "pos")));
        (Nolabel, Exp.ident ~loc (Located.mk ~loc (Lident "neg")))
      ] in
      Vb.mk ~loc (Pat.var ~loc str_loc) apply_blame
    in

    (* let [] f = f p in
       let f = f neg pos in
       acc_body *)
    Exp.let_ ~loc Nonrecursive [shadow_f_vb] (
      Exp.let_ ~loc Nonrecursive [blame_f_vb] acc_body
    )

  ) body params

let build_wrapped_body ~loc (params : (string * core_type) list) (rettype : core_type) body =
  (* we wrap the body into

    where cases are ContractCheck_params and ContractCheck__ret
    params holds the type constraints, which have the attributes
    the predicate for params are in [@pre] and for retrun value in [@post]

    ContractCheck_params p -> Some(fun k ->
      let allow = match predicate p with
        | v -> v
      in
      if allow
      then continue k ()
      else discontinue k (Blame pos)
    )

    ContractCheck__ret p -> Some(fun k ->
      let allow = match predicate p with
        | v -> v
      in
      if allow
      then continue k ()
      else discontinue k (Blame neg)
    )

    ====

    (* update the pos, neg here because params are flipped *)
    let pos, neg = neg, pos in
    let handler =
      {
        retc = (fun x -> x)
        exnc = (fun e -> raise e)
        effc = fun (type e) (eff : e Effect.t) ->
          match eff with
          | cases
      }
    in
    Effect.Deep.match_with (fun () ->
      Effect.perform (LocalContract (ContractCheck_param1));
      Effect.perform (LocalContract (ContractCheck_param2));
      let __ret__ = body in
      Effect.perform (LocalContract (ContractCheck__ret __ret__));
      __ret__
    ) ()
    handler
   *)
  (* 1. Generate Case for a Single Parameter/Return *)
  let make_check_case name ct is_post =
    let pred = get_predicate ~loc (if is_post then "post" else "pre") ct in
    let blame_label = if is_post then "neg" else "pos" in
    let attr_label = if is_post then "post" else "pre" in

    let eff_name = if is_post then "ContractCheck__ret" else "ContractCheck_" ^ name in
    let lid = Ldot (Lident "LocalContract", eff_name) in
    let lhs = Pat.construct ~loc (Located.mk ~loc lid) (Some [%pat? p]) in

    let pred = get_predicate ~loc attr_label ct in

    (* RHS: Some (fun k -> if (pred p) then continue k () else discontinue ...) *)
    let rhs = [%expr Some (fun k ->
      let allow = [%e Exp.apply ~loc pred [Nolabel, [%expr p]]] in
      if allow then
        Effect.Deep.continue k ()
      else
        Effect.Deep.discontinue k (Blame [%e Exp.ident ~loc (Located.mk ~loc (Lident blame_label))])
    )] in
    Exp.case lhs rhs
  in

  let non_function_params_map = List.filter (fun (_, ct) -> not (is_function ct)) params in
  let is_function_params_map = List.filter (fun (_, ct) -> is_function ct) params in

  (* 2. Build the Handlers list *)
  let param_cases = List.map (fun (name, ct) -> make_check_case name ct false) non_function_params_map in
  let ret_case = make_check_case "_ret" rettype true in
  let default_case = Exp.case [%pat? _] [%expr None] in

  (* 3. Build the Instrumented Body (The 'fun () -> ...' block) *)
  let performs = List.map (fun (name, _) ->
    let eff_name = "ContractCheck_" ^ name in
    let lid = Ldot (Lident "LocalContract", eff_name) in
    let constr = Located.mk ~loc lid in
    let arg = Exp.ident ~loc (Located.mk ~loc (Lident name)) in
    [%expr Effect.perform ([%e Exp.construct ~loc constr (Some arg)])]
  ) non_function_params_map in

  let instrumented_body =
    let return_instrumentation = [%expr
      let __ret__ = [%e body] in
      Effect.perform (LocalContract.ContractCheck__ret __ret__);
      __ret__
    ] in
    List.fold_right (fun p acc -> Exp.sequence ~loc p acc) performs return_instrumentation
  in

  let build_effc_expr ~loc param_cases ret_case default_case =
    let all_cases = param_cases @ [ret_case; default_case] in

    (* 1. Build the match WITHOUT (type a).
       We use 'eff' which will be bound by the arrow type later. *)
    let match_eff = Exp.match_ ~loc [%expr eff] all_cases in
    let match_constraint =
      [%type: ( (e, b) Effect.Deep.continuation -> b) option]
    in
    let constrained_match = Exp.constraint_ ~loc match_eff match_constraint in

    [%expr fun (type b) (type e) (eff : e Effect.t) -> [%e constrained_match]]
  in

  (* function params *)


  let effc_fn = build_effc_expr ~loc param_cases ret_case default_case in

  let main_catcher =
    [%expr
      let __contract_handler__ = {
        Effect.Deep.retc = (fun x -> x);
        exnc = (fun e -> raise e);
        effc = [%e effc_fn];
      } in
      Effect.Deep.match_with (fun () -> [%e instrumented_body]) () __contract_handler__
    ]
  in

  build_function_wrappers ~loc is_function_params_map main_catcher

let wrap_with_effects (vb : value_binding) params rettype body_expr =
  let pparams =
    List.map (fun param ->
      match param.pparam_desc with
      | Pparam_newtype _ -> None
      | Pparam_val (_, _, pat) ->
          match pat.ppat_desc with
          | Ppat_constraint (p, ct) ->
              begin
                match p.ppat_desc with
                | Ppat_var name -> Some (name.txt, ct)
                | _ -> None
              end
          | _ -> None
    ) params
    |> List.filter_map (fun x -> x)
  in

  let non_function_params_map = List.filter (fun (_, ct) -> not (is_function ct)) pparams in
  let is_function_params_map = List.filter (fun (_, ct) -> is_function ct) pparams in

  let loc = vb.pvb_loc in
  let contract_module = build_contract_module ~loc non_function_params_map rettype in

  let wrapped_body = build_wrapped_body ~loc pparams rettype body_expr in

  let new_body = pexp_letmodule ~loc
    (Located.mk ~loc (Some "LocalContract"))
    contract_module
    [%expr
      let pos, neg = neg, pos in
      Printf.printf "%s: pos=%s neg=%s\n" __FUNCTION__ pos neg;
      [%e wrapped_body]
    ]
  in

  let pos_param = mk_string_param ~loc "pos" in
  let neg_param = mk_string_param ~loc "neg" in
  let new_params = pos_param :: neg_param :: params in

  pexp_function ~loc new_params (Some (Pconstraint rettype)) (Pfunction_body new_body)

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
      | Pstr_value (recursive, [binding]) ->
          (* this is the outermost binding *)
          let var_name = Option.get (binding_name binding) in

          let module_name = match cctx.current with
            | Some n -> n
            | None -> root_module_of_loc binding.pvb_loc
          in
          let cctx = {cctx with current = Some module_name} in

          (* if recursive, then what? *)
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
        let rec core_type_of_contract ~loc (c : contract) : core_type =
          let mk_constr name = Ast_builder.Default.ptyp_constr ~loc { txt = Lident name; loc } [] in
          match c with
          | Flat s -> mk_constr s
          | Dependent s -> mk_constr s
          | Trace (s1, _s2) -> mk_constr s1 (* Adjust depending on how you represent Trace in AST *)
          | Function (dom, rng) ->
              Ast_builder.Default.ptyp_arrow ~loc Nolabel
                (core_type_of_contract ~loc dom)
                (core_type_of_contract ~loc rng)
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
                   let p_expr = Ast_builder.Default.pexp_ident ~loc { txt = Lident p1_name; loc } in

                   let attr = {
                     attr_name = { txt = "contract"; loc };
                     attr_payload = PTyp (core_type_of_contract ~loc dom);
                     attr_loc = loc;
                   } in

                   let vb_with_attr = {
                     (Ast_builder.Default.value_binding ~loc ~pat:p_pat ~expr:p_expr)
                     with pvb_attributes = [attr]
                   } in
                   let vb_clean = Ast_builder.Default.value_binding ~loc ~pat:p_pat ~expr:p_expr in

                   Ast_builder.Default.pexp_let ~loc Nonrecursive [vb_with_attr]
                     (Ast_builder.Default.pexp_let ~loc Nonrecursive [vb_clean] inner_body)

               | Some (p1_name, None) ->
                   (* FALLBACK: The user wrote `fun x ->` without a type.
                      Fallback to generating the type from the contract ADT's `dom`. *)
                   let p_pat = Ast_builder.Default.ppat_var ~loc { txt = p1_name; loc } in
                   let p_expr = Ast_builder.Default.pexp_ident ~loc { txt = Lident p1_name; loc } in

                   let attr = {
                     attr_name = { txt = "contract"; loc };
                     attr_payload = PTyp (core_type_of_contract ~loc dom);
                     attr_loc = loc;
                   } in

                   let vb_with_attr = {
                     (Ast_builder.Default.value_binding ~loc ~pat:p_pat ~expr:p_expr)
                     with pvb_attributes = [attr]
                   } in
                   let vb_clean = Ast_builder.Default.value_binding ~loc ~pat:p_pat ~expr:p_expr in

                   Ast_builder.Default.pexp_let ~loc Nonrecursive [vb_with_attr]
                     (Ast_builder.Default.pexp_let ~loc Nonrecursive [vb_clean] inner_body)

               | None -> inner_body) (* Fallback if parameter name is wild/unextractable *)

          (* Case 2: Base case. No more parameters, wrap the return value with the remaining rng *)
          | final_contract, [] ->
              let ret_pat = Ast_builder.Default.ppat_var ~loc { txt = "__ret__"; loc } in
              let ret_expr = Ast_builder.Default.pexp_ident ~loc { txt = Lident "__ret__"; loc } in

              let attr = {
                attr_name = { txt = "contract"; loc };
                attr_payload = PTyp (core_type_of_contract ~loc final_contract);
                attr_loc = loc;
              } in

              let ret_vb = {
                (Ast_builder.Default.value_binding ~loc ~pat:ret_pat ~expr:acc_body)
                with pvb_attributes = [attr]
              } in

              (* let [@contract: rng] __ret__ = body in __ret__ *)
              Ast_builder.Default.pexp_let ~loc Nonrecursive [ret_vb] ret_expr

          (* Edge case: Arity mismatch between contract and parameters *)
          | _, _ -> acc_body
        in

        (* 1. Build the newly wrapped body *)
        let wrapped_body = build_body contract params body in

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
          pvb_expr = self#expression ctx new_body;
          pvb_attributes = [];
        }

    | _ ->
        vb
        (* let t = Ast_builder.Default.ptyp_constr
          ~loc { txt = Lident "int"; loc } []
        in
        { vb with
          pvb_expr = wrap_with_effects vb [] t vb.pvb_expr;
          pvb_attributes = [];
        } *)

  method! value_binding ctx vb =
    let var_name = Option.get (binding_name vb) in
    let contract = get_contract_payload vb.pvb_attributes in
    match contract with
    | Some contract ->
        Printf.eprintf "value binding contract %s\n" (show_contract contract);

        (* if value is a function, also remove the arguments shadow monitor names *)
        let ctx = ctx in

        (* we don't filter out the same name here, shadow valid after this binding *)
        Printf.eprintf "%s (%s) current context is %s\n"
          var_name (Option.get ctx.current) (show_rewrite_context ctx);

        let new_expr = self#expression ctx vb.pvb_expr in

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

        self#transform_contract_wrapper ctx contract { vb with pvb_expr = new_expr }

    | None ->
        let new_expr = self#expression ctx vb.pvb_expr in
        { vb with pvb_expr = new_expr }

  method! expression ctx expr =
    match expr.pexp_desc with
    | Pexp_let (rec_flag, bindings, body) ->
        let (new_ctx, new_bindings) = List.fold_left (fun (cctx, new_bindings) binding ->
          let new_binding = self#value_binding cctx binding in
          let var_name = (binding_name new_binding) in
          let new_monitor = Option.bind var_name (fun name ->
            if is_fixed binding
            then Some { name; typ = Nested (Option.value ~default:"" cctx.current) }
            else None
          )
          in
          let new_monitors = match new_monitor with
          | Some m -> m :: cctx.monitors
          | None -> cctx.monitors
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
            Printf.eprintf "fix reference %s (%s)\n" name m;
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
