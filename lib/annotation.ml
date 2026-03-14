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

type contract =
  | Flat of string
  | Dependent of string
  | Function of contract * contract
  | Trace of string * string
  [@@deriving show]

let lident_eq name { txt; loc; } =
  match txt with
  | Lident n -> n = name
  | _ -> false

let name_from_lident { txt; loc; } =
  match txt with
  | Lident n -> n
  | _ -> ""

let is_dep = lident_eq "dep"
let is_trace = lident_eq "trace"

let rec parse_contract_annotations (typ : core_type) : contract option =
  match typ.ptyp_desc with
  | Ptyp_constr (name, []) -> Some (Flat (name_from_lident name))
  | Ptyp_constr (name, [predicate]) when is_dep name ->
      begin
      match parse_contract_annotations predicate with
      | Some (Flat n) -> Some (Dependent n)
      | _ -> None
      end
  | Ptyp_constr (name, [constructor; predicate]) when is_trace name ->
      begin
      match (parse_contract_annotations constructor, parse_contract_annotations predicate) with
      | (Some (Flat c), Some (Flat p)) -> Some (Trace (c, p))
      | _ -> None
      end
  | Ptyp_arrow (_, dom, rng) ->
      begin
      match (parse_contract_annotations dom, parse_contract_annotations rng) with
      | (Some dom, Some rng) -> Some (Function (dom, rng))
      | _ -> None
      end
  | _ ->
      None

let get_contract_payload attrs =
  let rec loop = function
    | [] -> None
    | attr :: rest ->
        if attr.attr_name.txt = "contract" then
          (* Match the `PTyp` payload and extract the identifier string *)

          match attr.attr_payload with
          | PTyp t -> parse_contract_annotations t
          | _ -> None
        else
          loop rest
  in
  loop attrs
