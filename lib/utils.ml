open Ppxlib
let debug_vb_source (vb : value_binding) =
  let loc = vb.pvb_loc in

  (* 1. Wrap the binding in a dummy `let` structure item *)
  let dummy_str_item = Ast_builder.Default.pstr_value ~loc Nonrecursive [vb] in

  (* 2. Print the list of structure items (which is just our one item) *)
  let source_code = Format.asprintf "%a" Pprintast.structure [dummy_str_item] in

  Format.eprintf "--- VALUE BINDING SOURCE ---@.%s@.----------------------------@." source_code

let debug_expr_source (expr : expression) =
  let source_code = Format.asprintf "%a" Pprintast.expression expr in
  Format.eprintf "--- EXPRESSION SOURCE ---@.%s@.-------------------------@." source_code

let rec extract_name_from_pat pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | Ppat_constraint (inner_pat, _) -> extract_name_from_pat inner_pat
  | Ppat_alias (inner_pat, _) -> extract_name_from_pat inner_pat
  | Ppat_construct (_, Some (_, inner_pat)) -> extract_name_from_pat inner_pat
  | Ppat_variant (_, Some inner_pat) -> extract_name_from_pat inner_pat
  | _ -> "unknown_var"

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

let rec unroll_n_arrows ~loc n typ =
  if n <= 0 then ([], typ)
  else match typ.ptyp_desc with
  | Ptyp_arrow (_, t1, t2) ->
      let rest, ret = unroll_n_arrows ~loc (n - 1) t2 in
      (t1 :: rest, ret)
  | Ptyp_poly (_, t) -> unroll_n_arrows ~loc n t
  | _ ->
      Location.raise_errorf ~loc
        "Type signature does not have enough arrows to match the contract's argument count"
