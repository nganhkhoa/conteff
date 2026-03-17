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
let trace_anno = "trace"

let contract_anno = "contract"
let contract_arg_anno = contract_anno ^ "_arg"
let contract_ret_anno = contract_anno ^ "_ret"
let contract_dep_anno = contract_anno ^ "_dep"
let contract_trace_anno = contract_anno ^ "_trace"

(* we keep the location of the flat contract
   so that when we use it to check predicate
   it can type check and throw the error at predicate
   idk about function contract though ?
   redesign when needed

   maybe all of them need location?
 *)
type contract =
  | Flat of string
  | Dependent of contract * contract
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

let is_dep = lident_eq dep_anno
let is_trace = lident_eq trace_anno

let rec parse_contract_annotations (typ : core_type) : contract option =
  match typ.ptyp_desc with
  | Ptyp_constr (name, []) -> Some (Flat (name_from_lident name))
  | Ptyp_constr (name, [constructor; predicate]) when is_trace name ->
      begin match (parse_contract_annotations constructor, parse_contract_annotations predicate) with
      | (Some (Flat c), Some (Flat p)) -> Some (Trace (c, p))
      | _ -> None
      end
  | Ptyp_arrow (_, dom, rng) ->
      begin match parse_contract_annotations dom with
      | None -> None
      | Some parsed_dom ->
          begin match rng.ptyp_desc with
          | Ptyp_constr (name, [actual_rng]) when is_dep name ->
              begin match parse_contract_annotations actual_rng with
              (* TODO: can we have a trace here? *)
              | Some parsed_rng -> Some (Dependent (parsed_dom, parsed_rng))
              | _ -> None
              end

          | _ ->
              begin match parse_contract_annotations rng with
              | Some parsed_rng -> Some (Function (parsed_dom, parsed_rng))
              | None -> None
              end
          end
      end
  | _ -> None

let rec get_contract_payload = function
  | [] -> None
  | attr :: rest ->
      if attr.attr_name.txt = contract_anno then
        match attr.attr_payload with
        | PTyp t -> parse_contract_annotations t
        | _ -> None
      else
        get_contract_payload rest

let rec get_contract_attr = function
  | [] -> None
  | attr :: rest ->
      if attr.attr_name.txt = contract_anno then
        Some attr
      else
        get_contract_attr rest

let rec core_type_of_contract ~loc (c : contract) : core_type =
  let mk_constr name = Ast_builder.Default.ptyp_constr ~loc
    { txt = Lident name; loc } []
  in

  match c with
  | Flat s -> mk_constr s
  | Trace (s1, s2) ->
      Ast_builder.Default.ptyp_constr ~loc
        { txt = Lident trace_anno; loc }
        [mk_constr s1; mk_constr s2]
  | Function (dom, rng) ->
      Ast_builder.Default.ptyp_arrow ~loc Nolabel
        (core_type_of_contract ~loc dom)
        (core_type_of_contract ~loc rng)
  | Dependent (dom, rng) ->
      let dom_ct = core_type_of_contract ~loc dom in
      let rng_ct = core_type_of_contract ~loc rng in
      let rng_dep =
        Ast_builder.Default.ptyp_constr ~loc
          { txt = Lident dep_anno; loc }
          [rng_ct]
      in
      Ast_builder.Default.ptyp_arrow ~loc Nolabel dom_ct rng_dep

let remove_contract_attributes (attrs : attributes) : attributes =
  let is_contract_attribute (attr : attribute) =
    List.mem attr.attr_name.txt
    [ contract_anno;
      contract_arg_anno;
      contract_ret_anno;
      contract_dep_anno;
      contract_trace_anno;
    ]
  in
  List.filter (fun attr -> not (is_contract_attribute attr)) attrs

let contract_as_attribute ~loc name contract =
  match contract with
  (* | Flat (pred, Some cloc) ->
      (* override location to this contract location *)
      {
        attr_name = { txt = name; loc };
        attr_payload = PTyp (core_type_of_contract ~cloc contract);
        attr_loc = loc;
      } *)
  | _ ->
      {
        attr_name = { txt = name; loc };
        attr_payload = PTyp (core_type_of_contract ~loc contract);
        attr_loc = loc;
      }
