open Ppxlib
open Ast_builder.Default

open Annotation

(* Helper to match your path generation *)
let effect_t loc = { txt = Ldot (Lident "Effect", "t"); loc }

type eff_decl = {
  eff_name : string;
  payload_types : core_type list;
  ret_type : core_type;
}

let rec build_effects ~loc prefix deps contract ct : eff_decl list =
  let string_typ = ptyp_constr ~loc { txt = Lident "string"; loc } [] in

  match contract with
  (* Base case: Emit the flat effect *)
  | Flat _ ->
      [ { eff_name = prefix;
          payload_types = string_typ :: (deps @ [ct]);
          ret_type = ct } ]

  (* Standard Function: Map arguments without accumulating dependencies *)
  | Function (arg_contracts, ret_contract) ->
      let arg_cts, ret_ct = unroll_arrow ct in
      let arg_effs =
        List.mapi (fun i (arg_c, arg_t) ->
          let arg_prefix = prefix ^ "'p" ^ string_of_int (i + 1) in
          build_effects ~loc arg_prefix [] arg_c arg_t
        ) (List.combine arg_contracts arg_cts)
        |> List.flatten
      in
      let ret_effs = build_effects ~loc (prefix ^ "'r") [] ret_contract ret_ct in
      arg_effs @ ret_effs

  (* Dependent Function: Accumulate previous arguments as dependencies *)
  | Dependent (arg_contracts, ret_contract) ->
      let arg_cts, ret_ct = unroll_arrow ct in

      let rec process_deps i acc_deps contracts types =
        match contracts, types with
        | [], [] -> []
        | c :: cs, t :: ts ->
            let arg_prefix = prefix ^ "'p" ^ string_of_int i in
            let effs = build_effects ~loc arg_prefix acc_deps c t in
            effs @ process_deps (i + 1) (acc_deps @ [t]) cs ts
        | _ -> Location.raise_errorf ~loc "Contract and Type argument mismatch"
      in

      let arg_effs = process_deps 1 deps arg_contracts arg_cts in
      (* The return contract receives ALL arguments as dependencies *)
      let final_deps = deps @ arg_cts in
      let ret_effs = build_effects ~loc (prefix ^ "'r") final_deps ret_contract ret_ct in
      arg_effs @ ret_effs

let generate_contract_module ~loc eff_declarations body_expr =
  let open Ast_builder.Default in

  (* Define the type parameters for _ Effect.t *)
  let params = [ (ptyp_any ~loc, (NoVariance, NoInjectivity)) ] in

  (* Map our flattened effect list into AST extension constructors *)
  let constructors =
    List.map (fun decl ->
      let eff_constr_name = "C'" ^ decl.eff_name in

      (* 1. Build the tuple type for the constructor argument.
         Since your payload is `(string * type * ...)`, we pack the list of
         types into a single ptyp_tuple. *)
      let payload_tuple = ptyp_tuple ~loc decl.payload_types in

      (* 2. Build the exact return type: <ret_type> Effect.t *)
      let effect_typ = ptyp_constr ~loc (effect_t loc) [decl.ret_type] in

      (* 3. Use your exact Pext_decl pattern *)
      extension_constructor ~loc
        ~name:{ txt = eff_constr_name; loc }
        ~kind:(Pext_decl (
          [], (* No existential type variables *)
          Pcstr_tuple [payload_tuple], (* The single argument is our tuple *)
          Some effect_typ (* The GADT return type *)
        ))
    ) eff_declarations
  in

  (* 4. Construct `type _ Effect.t += ...` *)
  let typext =
    type_extension ~loc
      ~path:(effect_t loc)
      ~params
      ~constructors
      ~private_:Public
  in

  (* 5. Wrap it in a structure *)
  let module_expr = pmod_structure ~loc [ pstr_typext ~loc typext ] in

  (* 6. Wrap the original function body inside `let module Contract = ... in` *)
  pexp_letmodule ~loc { txt = Some "Contract"; loc } module_expr body_expr

type expanded_param = {
  eff_name : string;      (* e.g., "x'", "g'p1'", "g'r'" *)
  func_name : string;     (* e.g., "x", "g'p1", "g" *)
  deps : core_type list;  (* Injected dependencies if dependent *)
  ret_type : core_type;
  is_func : bool;         (* Is this a function proxy (an 'r' wrapper)? *)
  arg_wrappers : string list; (* Names of wrappers to apply to its arguments *)
}

(* Returns: (wrapper_name, list_of_parameters) *)
let rec expand_param prefix deps_for_ret contract ct : string * expanded_param list =
  let args_cts, ret_ct = unroll_arrow ct in

  match contract, args_cts with
  | (Flat _, []) ->
      (* BASE CASE: Append the prime mark *)
      let wrapper_name = prefix ^ "'" in
      let ep = {
        eff_name = wrapper_name;
        func_name = prefix;
        deps = deps_for_ret;
        ret_type = ct;
        is_func = false;
        arg_wrappers = [];
      } in
      (wrapper_name, [ep])

  | (Function (arg_contracts, _), _) | (Dependent (arg_contracts, _), _) ->
      let is_dependent = match contract with Dependent _ -> true | _ -> false in

      (* Recursively get the wrapper names for all arguments *)
      let rec process_args i contracts types =
        match contracts, types with
        | [], [] -> ([], [])
        | c :: cs, t :: ts ->
            let arg_prefix = prefix ^ "'p" ^ string_of_int i in
            let arg_wrapper_name, effs = expand_param arg_prefix [] c t in
            let rest_wrappers, rest_effs = process_args (i + 1) cs ts in
            (arg_wrapper_name :: rest_wrappers, effs @ rest_effs)
        | _ -> Location.raise_errorf ~loc:ct.ptyp_loc "Mismatch"
      in
      let arg_wrappers, expanded_args = process_args 1 arg_contracts args_cts in

      let ret_deps = if is_dependent then args_cts else [] in

      (* FUNCTION PROXY: The return effect acts as the wrapper for the whole function *)
      let wrapper_name = prefix ^ "'r'" in
      let ep = {
        eff_name = wrapper_name;
        func_name = prefix;
        deps = ret_deps;
        ret_type = ret_ct;
        is_func = true;
        arg_wrappers = arg_wrappers;
      } in
      (wrapper_name, expanded_args @ [ep])

  | _ -> Location.raise_errorf ~loc:ct.ptyp_loc "Structure mismatch"


let unit_pat ~loc = ppat_construct ~loc {txt=Lident "()"; loc} None
let unit_exp ~loc = pexp_construct ~loc {txt=Lident "()"; loc} None

let is_arrow ct = match ct.ptyp_desc with Ptyp_arrow _ -> true | _ -> false

let generate_wrapper_binding ~loc ep =
  let open Ast_builder.Default in
  let pos_pat = ppat_var ~loc {txt="pos"; loc}
  and neg_pat = ppat_var ~loc {txt="neg"; loc}
  and cloc_pat = ppat_var ~loc {txt="cloc"; loc}
  and val_pat = ppat_var ~loc {txt=ep.func_name; loc} in

  let pos_exp = pexp_ident ~loc {txt=Lident "pos"; loc}
  and neg_exp = pexp_ident ~loc {txt=Lident "neg"; loc}
  and cloc_exp = pexp_ident ~loc {txt=Lident "cloc"; loc}
  and val_exp = pexp_ident ~loc {txt=Lident ep.func_name; loc} in

  let effect_constr = {txt=Ldot(Lident "Contract", "C'" ^ ep.eff_name); loc} in

  let body_expr =
    if not ep.is_func then
      (* --- BASE VALUE WRAPPER --- *)
      let payload = pexp_tuple ~loc [pos_exp; val_exp] in
      let eff_call = pexp_construct ~loc effect_constr (Some payload) in
      pexp_apply ~loc (pexp_ident ~loc {txt=Ldot(Lident "Effect", "perform"); loc}) [(Nolabel, eff_call)]

    else
      (* --- FUNCTION PROXY WRAPPER (e.g., g'p2'r') --- *)
      let arg_names = List.mapi (fun i _ -> ep.func_name ^ "'p" ^ string_of_int (i+1)) ep.arg_wrappers in
      let is_dep = List.length ep.deps > 0 in
      let body_var_name = "body" in
      let body_exp = pexp_ident ~loc {txt=Lident body_var_name; loc} in

      (* 1. Call dep() if base type, else pass dep directly *)
      let payload_exprs =
        if is_dep then
          let dep_args = List.map2 (fun name typ ->
            let dep_ident = pexp_ident ~loc {txt=Lident (name ^ "_dep"); loc} in
            if is_arrow typ then dep_ident else pexp_apply ~loc dep_ident [(Nolabel, unit_exp ~loc)]
          ) arg_names ep.deps in
          pos_exp :: dep_args @ [body_exp]
        else [pos_exp; body_exp]
      in
      let perform_call = pexp_apply ~loc (pexp_ident ~loc {txt=Ldot(Lident "Effect", "perform"); loc})
        [(Nolabel, pexp_construct ~loc effect_constr (Some (pexp_tuple ~loc payload_exprs)))] in

      (* 2. let body = g p1 p2 ... in ... *)
      let func_apply = pexp_apply ~loc val_exp (List.map (fun n -> (Nolabel, pexp_ident ~loc {txt=Lident n; loc})) arg_names) in
      let eval_body = pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:(ppat_var ~loc {txt=body_var_name; loc}) ~expr:func_apply] perform_call in

      (* 3. let p1 = p1' neg pos cloc p1 in ... *)
      let apply_wrappers = List.fold_right2 (fun name wrapper acc ->
        let call = pexp_apply ~loc (pexp_ident ~loc {txt=Lident wrapper; loc}) [
          (Nolabel, neg_exp); (Nolabel, pos_exp); (Nolabel, cloc_exp); (Nolabel, pexp_ident ~loc {txt=Lident name; loc})
        ] in
        pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:(ppat_var ~loc {txt=name; loc}) ~expr:call] acc
      ) arg_names ep.arg_wrappers eval_body in

      (* 4. let p1_dep = fun () -> p1' neg cloc cloc p1 in ... *)
      let apply_deps = if not is_dep then apply_wrappers else
        List.fold_right (fun ((name, wrapper), typ) acc ->
          let call = pexp_apply ~loc (pexp_ident ~loc {txt=Lident wrapper; loc}) [
            (Nolabel, neg_exp); (Nolabel, cloc_exp); (Nolabel, cloc_exp); (Nolabel, pexp_ident ~loc {txt=Lident name; loc})
          ] in
          let expr = if is_arrow typ then call else pexp_fun ~loc Nolabel None (unit_pat ~loc) call in
          pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:(ppat_var ~loc {txt=name ^ "_dep"; loc}) ~expr] acc
        ) (List.combine (List.combine arg_names ep.arg_wrappers) ep.deps) apply_wrappers
      in

      (* 5. fun p1 p2 -> ... *)
      List.fold_right (fun name acc -> pexp_fun ~loc Nolabel None (ppat_var ~loc {txt=name; loc}) acc) arg_names apply_deps
  in

  (* THE FINAL LAMBDA *)
  let final_lambda =
    pexp_fun ~loc Nolabel None pos_pat (
      pexp_fun ~loc Nolabel None neg_pat (
        pexp_fun ~loc Nolabel None cloc_pat (
          pexp_fun ~loc Nolabel None val_pat body_expr
        )))
  in

  (* FIX: We must return a value_binding, not just the lambda expression *)
  value_binding ~loc
    ~pat:(ppat_var ~loc {txt=ep.eff_name; loc})
    ~expr:final_lambda

let wrap_main_body ~loc original_body param_names wrappers types is_dep =
  let pos_exp = pexp_ident ~loc {txt=Lident "pos"; loc}
  and neg_exp = pexp_ident ~loc {txt=Lident "neg"; loc}
  and cloc_exp = pexp_ident ~loc {txt=Lident "cloc"; loc} in

  let body_var_name = "body" in
  let body_exp = pexp_ident ~loc {txt=Lident body_var_name; loc} in

  (* 1. Effect.perform C'r' payload *)
  let payload_exprs =
    if is_dep then
      let dep_args = List.map2 (fun name typ ->
        let dep_ident = pexp_ident ~loc {txt=Lident (name ^ "_dep"); loc} in
        if is_arrow typ then dep_ident else pexp_apply ~loc dep_ident [(Nolabel, unit_exp ~loc)]
      ) param_names types in
      pos_exp :: dep_args @ [body_exp]
    else [pos_exp; body_exp]
  in
  let eff_call = pexp_construct ~loc {txt=Ldot(Lident "Contract", "C'r'"); loc} (Some (pexp_tuple ~loc payload_exprs)) in
  let perform_call = pexp_apply ~loc (pexp_ident ~loc {txt=Ldot(Lident "Effect", "perform"); loc}) [(Nolabel, eff_call)] in

  (* 2. let body = <original_body> in ... *)
  let eval_body = pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:(ppat_var ~loc {txt=body_var_name; loc}) ~expr:original_body] perform_call in

  (* 3. let x = x' neg pos cloc x in ... *)
  let apply_wrappers = List.fold_right2 (fun name wrapper acc ->
    let call = pexp_apply ~loc (pexp_ident ~loc {txt=Lident wrapper; loc}) [
      (Nolabel, neg_exp); (Nolabel, pos_exp); (Nolabel, cloc_exp); (Nolabel, pexp_ident ~loc {txt=Lident name; loc})
    ] in
    pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:(ppat_var ~loc {txt=name; loc}) ~expr:call] acc
  ) param_names wrappers eval_body in

  (* 4. let x_dep = ... (if dependent) *)
  if not is_dep
  then apply_wrappers
  else
    (* FIX: Changed fold_right2 to fold_right here as well *)
    List.fold_right (fun ((name, wrapper), typ) acc ->
      let call = pexp_apply ~loc (pexp_ident ~loc {txt=Lident wrapper; loc}) [
        (Nolabel, neg_exp); (Nolabel, cloc_exp); (Nolabel, cloc_exp); (Nolabel, pexp_ident ~loc {txt=Lident name; loc})
      ] in
      let expr = if is_arrow typ then call else pexp_fun ~loc Nolabel None (unit_pat ~loc) call in
      pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:(ppat_var ~loc {txt=name ^ "_dep"; loc}) ~expr] acc
    ) (List.combine (List.combine param_names wrappers) types) apply_wrappers
