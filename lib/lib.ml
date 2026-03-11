open Effect
open Ppxlib
open Ast_helper
open Ast_builder.Default

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

let transform_contract_wrapper (vb : value_binding) =
  match vb.pvb_expr.pexp_desc with
  (* for now only transform a function annotated with [@contract]
     we require type signature, because the predicate contract will be
     attached to the type signature
     we only work for body expression, for function match cases, we ignore
  *)
  | Pexp_function (params, Some (Pconstraint rettype), Pfunction_body body) ->
      { vb with
        pvb_expr = wrap_with_effects vb params rettype body;
        pvb_attributes = [];
      }

  | _ -> vb

let expander =
  object
    inherit Ast_traverse.map as super

    method! value_binding vb =
      (* Format.eprintf "Transforming: %a@." Pprintast.expression vb.pvb_expr; *)
      match Attribute.get contract_attr vb with
      | Some () ->
          let transformed_vb = transform_contract_wrapper vb in
          let recursive_expr = super#expression transformed_vb.pvb_expr in
          { transformed_vb with pvb_expr = recursive_expr }
      | None ->
          super#value_binding vb
  end

(* 3. Register the transformation *)
let () =
  Driver.register_transformation
    ~impl:expander#structure
    "cueh.ppx_sceh"
