open Effect
open Ppxlib
open Ast_helper
open Ast_builder.Default

open Utils

(* Helper: Extract do_contract name from attributes *)
let get_do_contract_data attr =
  if attr.attr_name.txt = "do_contract" then
    match attr.attr_payload with
    | PTyp { ptyp_desc = Ptyp_tuple [t1; t2]; _ } ->
        let extract_id t = match t.ptyp_desc with Ptyp_constr ({txt = Lident n; _}, []) -> Some n | _ -> None in
        (match extract_id t1, extract_id t2 with
         | Some p, Some e -> Some (p, e)
         | _ -> None)
    | _ -> None
  else None

(* Logic to build: Check'<name> : (string * type) -> type Effect.t *)
let mk_effect_constructor ~loc name typ =
  let args = Pcstr_tuple [ Typ.constr ~loc {txt = Lident "string"; loc} []; typ ] in
  let ret_typ = Typ.constr ~loc {txt = Lident "Effect.t"; loc} [typ] in
  Te.decl ~loc ~args ?res:(Some ret_typ) { txt = "Check'" ^ name; loc }

let mk_handler_case ~loc predicate_name effect_name =
  let mk_id txt = { txt = Lident txt; loc } in
  let mk_ident txt = Exp.ident ~loc (mk_id txt) in
  let mk_var txt = ppat_var ~loc { txt; loc } in

  (* Pattern constructors take longident loc, so mk_id is correct here *)
  let pattern = ppat_construct ~loc (mk_id ("Check'" ^ effect_name))
    (Some (ppat_tuple ~loc [mk_var "label"; mk_var "arg"])) in

  let handler_body =
    Exp.apply ~loc (mk_ident "Some") [ (Nolabel,
      Exp.fun_ ~loc Nolabel None (mk_var "k") (
        Exp.ifthenelse ~loc
          (Exp.apply ~loc (mk_ident predicate_name) [(Nolabel, mk_ident "arg")])
          (Exp.apply ~loc (mk_ident "continue") [(Nolabel, mk_ident "k"); (Nolabel, mk_ident "arg")])
          (Some (Exp.apply ~loc (mk_ident "discontinue") [
             (Nolabel, mk_ident "k");
             (* Exp.construct takes a longident loc for the constructor name *)
             (Nolabel, Exp.construct ~loc (mk_id "Blame") (Some (mk_ident "label")))
          ]))
      )
    )]
  in
  Exp.case pattern handler_body

let gen_contract_checking_module ~loc bindings =
  let mk_id txt = { txt = Lident txt; loc } in
  let mk_ident txt = Exp.ident ~loc (mk_id txt) in
  let mk_var txt = ppat_var ~loc { txt; loc } in

  if bindings = [] then []
  else
    (* 1. Exception Blame of string *)
    let exc_blame =
      let constr = Te.decl ~loc
        ~args:(Pcstr_tuple [Typ.constr ~loc {txt=Lident "string"; loc} []])
        {txt="Blame"; loc}
      in

      (* Manually construct the Parsetree.type_exception record *)
      let type_exn : Parsetree.type_exception = {
        ptyexn_constructor = constr;
        ptyexn_loc = loc;
        ptyexn_attributes = [];
      } in

      Str.exception_ ~loc type_exn
    in

    (* 2. Type extension: type _ Effect.t += ... *)
    let constructors = List.map (fun (p, e, typ) -> mk_effect_constructor ~loc e typ) bindings in
    let type_ext = Te.mk ~loc ~params:[(Typ.any ~loc (), (NoVariance, NoInjectivity))] {txt=Lident "Effect.t"; loc} constructors |> Str.type_extension ~loc in

    (* 3. Handler: let rec contract_checking ... *)
    let cases = List.map (fun (p, e, _) -> mk_handler_case ~loc p e) bindings @ [Exp.case (ppat_any ~loc) (Exp.ident ~loc {txt=Lident "None"; loc})] in
    let handler_fun = Exp.fun_ ~loc Nolabel None (mk_var "eff") (Exp.match_ ~loc (mk_ident "eff") cases) in

    (* GADT Type Annotation for the handler *)
    let handler_typ = Typ.poly ~loc [{txt="a"; loc}; {txt="b"; loc}]
      (Typ.arrow ~loc Nolabel (Typ.constr ~loc {txt=Lident "Effect.t"; loc} [Typ.constr ~loc {txt=Lident "a"; loc} []])
        (Typ.constr ~loc {txt=Lident "option"; loc} [
          Typ.arrow ~loc Nolabel
            (Typ.constr ~loc {txt=Lident "Effect.Deep.continuation"; loc} [Typ.constr ~loc {txt=Lident "a"; loc} []; Typ.constr ~loc {txt=Lident "b"; loc} []])
            (Typ.constr ~loc {txt=Lident "b"; loc} [])
        ]))
    in

    let handler_vb = Vb.mk ~loc (ppat_constraint ~loc (mk_var "contract_checking") handler_typ) handler_fun in
    let handler_let = Str.value ~loc Nonrecursive [handler_vb] in

    [Str.module_ ~loc (Mb.mk ~loc {txt=Some "ContractChecking"; loc} (Mod.structure ~loc ([exc_blame; type_ext; Str.open_ ~loc (Opn.mk ~loc (Mod.ident ~loc {txt=Lident "Effect.Deep"; loc})); handler_let])))]

let deep_collect_effects = object
  inherit [(string * string * core_type) list] Ast_traverse.fold as super

  method! value_binding vb acc =
    let acc =
      match List.find_map get_do_contract_data vb.pvb_attributes with
      | Some (predicate, eff) ->
          (match extract_type_from_vb vb with
           | Some typ -> (predicate, eff, typ) :: acc
           | None -> acc)
      | None -> acc
    in
    super#value_binding vb acc

  method! module_binding _ acc = acc
end

let walker = object(self)
  inherit Ast_traverse.map as super

  method! structure (str : structure) =
    let loc = match str with h :: _ -> h.pstr_loc | [] -> Location.none in

    (* 1. Transform structure recursively *)
    let transformed_str = List.map (fun item ->
      match item.pstr_desc with
      | Pstr_module mb ->
          let new_expr = self#module_expr mb.pmb_expr in
          { item with pstr_desc = Pstr_module { mb with pmb_expr = new_expr } }
      | _ -> super#structure_item item
    ) str in

    (* 2. Deep Harvest with updated Tuple Data *)
    let local_effects = object
      inherit [(string * string * core_type) list] Ast_traverse.fold as super
      method! value_binding vb acc =
        let acc = match List.find_map get_do_contract_data vb.pvb_attributes with
          | Some (p, e) -> (match extract_type_from_vb vb with Some t -> (p, e, t) :: acc | _ -> acc)
          | _ -> acc in
        super#value_binding vb acc
      method! module_binding _ acc = acc
    end in

    let effects = local_effects#structure transformed_str [] in
    let unique_effects = List.sort_uniq (fun (_, e1, _) (_, e2, _) -> String.compare e1 e2) effects in

    (* 3. APPEND (not prepend) the ContractChecking module *)
    let effect_module = gen_contract_checking_module ~loc (List.rev unique_effects) in
    transformed_str @ effect_module

  method! module_expr m =
    match m.pmod_desc with
    | Pmod_structure str -> { m with pmod_desc = Pmod_structure (self#structure str) }
    | _ -> super#module_expr m
end
