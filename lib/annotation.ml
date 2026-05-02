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

let rec unroll_arrow (ct : core_type) : core_type list * core_type =
  match ct.ptyp_desc with
  | Ptyp_arrow (_, arg, ret) ->
      let (args, final_ret) = unroll_arrow ret in
      (arg :: args, final_ret)
  | _ -> ([], ct)

let rec parse_contract_type (ct : core_type) : contract =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "dep"; _ }, [ inner_ct ]) ->
      let args_cts, ret_ct = unroll_arrow inner_ct in
      let parsed_args = List.map parse_contract_type args_cts in
      let parsed_ret = parse_contract_type ret_ct in
      Dependent (parsed_args, parsed_ret)

  | Ptyp_arrow _ ->
      let args_cts, ret_ct = unroll_arrow ct in
      let parsed_args = List.map parse_contract_type args_cts in
      let parsed_ret = parse_contract_type ret_ct in
      Function (parsed_args, parsed_ret)

  | Ptyp_constr ({ txt = Lident name; _ }, []) ->
      Flat name

  | _ -> Location.raise_errorf ~loc:ct.ptyp_loc "Unsupported contract type syntax"

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
