open Ppxlib
open Ast_helper
open Ast_builder.Default

(* this file provides all related functionalities to parse
   the [@contract] annotation

   current design is using type annotation to declare the contract
   this should be consitent with functions as well

   [@contract : k1 -> k2 -> k3] these are 3 separated flat contracts

   [@contract : (k1 -> k2) -> k3] these are 2 contracts
   --> function receiving a function and return some basic value

   [@contract : k1 -> (k2 -> k3)] these are 2 contracts
   --> function receiving a basic value and return some function
 *)

let arg_anno = "arg"
let ret_anno = "ret"
let dep_anno = "dep"

let contract_anno = "contract"
let contract_arg_anno = contract_anno ^ "_arg"
let contract_ret_anno = contract_anno ^ "_ret"
let contract_dep_anno = contract_anno ^ "_dep"

(* we keep the location of the flat contract
   so that when we use it to check predicate
   it can type check and throw the error at predicate
   idk about function contract though ?
   redesign when needed

   maybe all of them need location?
 *)
type contract =
  | Flat of string
  | Dependent of contract list * contract
  | Function of contract list * contract
  [@@deriving show]

let lident_eq name { txt; loc; } =
  match txt with
  | Lident n -> n = name
  | _ -> false

let name_from_lident { txt; loc; } =
  match txt with
  | Lident n -> n
  | _ -> ""

let is_dep = lident_eq dep_anno

let rec unroll_arrow ct =
  match ct.ptyp_desc with
  (* Stop unrolling when we hit the 'ret' label *)
  | Ptyp_arrow (Labelled "ret", inner_ret_ct, _) ->
      ([], inner_ret_ct)

  (* Continue unrolling normal arguments *)
  | Ptyp_arrow (Nolabel, arg_ct, ret_ct) ->
      let args, final_ret = unroll_arrow ret_ct in
      (arg_ct :: args, final_ret)

  | _ -> ([], ct)

let rec parse_contract_type (ct : core_type) : contract =
  match ct.ptyp_desc with

  (* 1. Dependent Contract: Tagged with `as 'dep` *)
  | Ptyp_alias (inner_ct, { txt = "dep"; _ }) ->
      let args, ret = extract_func_args_and_ret inner_ct in
      Dependent (args, ret)

  (* 2. Function Contract: Standard arrow *)
  | Ptyp_arrow (Nolabel, _, _) ->
      let args, ret = extract_func_args_and_ret ct in
      Function (args, ret)

  (* 3. Basic Contract: Flat identifier *)
  | Ptyp_constr ({ txt = Lident name; _ }, []) ->
      Flat name

  | _ ->
      Location.raise_errorf ~loc:ct.ptyp_loc
        "Unsupported contract syntax. Expected ident, (args) -> ret, or (args) -> ret as 'dep"

and extract_func_args_and_ret (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_arrow (Nolabel, lhs_args, rhs_ret) ->
      let parsed_args =
        match lhs_args.ptyp_desc with
        (* Handle multiple arguments grouped in a tuple: (p1 * p2 * p3) *)
        | Ptyp_tuple args ->
            List.map parse_contract_type args

        (* Handle a single argument without a tuple: p1 *)
        | _ ->
            [parse_contract_type lhs_args]
      in
      let parsed_ret = parse_contract_type rhs_ret in
      (parsed_args, parsed_ret)

  | _ ->
      Location.raise_errorf ~loc:ct.ptyp_loc
        "Malformed function contract. Expected an arrow type."

let rec get_contract_payload = function
  | [] -> None
  | attr :: rest ->
      if attr.attr_name.txt = contract_anno then
        match attr.attr_payload with
        | PTyp t -> Some (parse_contract_type t)
        | _ -> None
      else
        get_contract_payload rest

let remove_contract_attributes (attrs : attributes) : attributes =
  let is_contract_attribute (attr : attribute) =
    List.mem attr.attr_name.txt
    [ contract_anno; ]
  in
  List.filter (fun attr -> not (is_contract_attribute attr)) attrs

let rec contract_to_core_type ~loc (c : contract) : core_type =
  (* Helper to generate boilerplate Parsetree nodes *)
  let mk_ct ptyp_desc =
    { ptyp_desc; ptyp_loc = loc; ptyp_loc_stack = []; ptyp_attributes = [] }
  in
  let mk_loc txt = { Location.txt; loc } in

  (* Helper to handle the tuple wrapping logic *)
  let build_args args =
    match args with
    | [] ->
        (* Fallback for zero arguments: map to standard OCaml `unit` *)
        mk_ct (Ptyp_constr (mk_loc (Longident.Lident "unit"), []))
    | [ single_arg ] ->
        (* Single arguments do not get wrapped in a tuple *)
        contract_to_core_type ~loc single_arg
    | multiple_args ->
        (* Multiple arguments get wrapped in a Ptyp_tuple *)
        let tuple_elements = List.map (contract_to_core_type ~loc) multiple_args in
        mk_ct (Ptyp_tuple tuple_elements)
  in

  match c with
  | Flat name ->
      mk_ct (Ptyp_constr (mk_loc (Longident.Lident name), []))

  | Function (args, ret) ->
      let lhs = build_args args in
      let rhs = contract_to_core_type ~loc ret in
      mk_ct (Ptyp_arrow (Nolabel, lhs, rhs))

  | Dependent (args, ret) ->
      let lhs = build_args args in
      let rhs = contract_to_core_type ~loc ret in
      (* Build the inner function... *)
      let inner_arrow = mk_ct (Ptyp_arrow (Nolabel, lhs, rhs)) in
      (* ...and wrap it in the 'dep alias *)
      mk_ct (Ptyp_alias (inner_arrow, mk_loc "dep"))
