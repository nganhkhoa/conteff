open Ppxlib
open Ast_helper
open Ast_builder.Default

let get_param_type (param : function_param) : core_type option =
  match param.pparam_desc with
  | Pparam_val (_, _, pat) ->
      begin match pat.ppat_desc with
      | Ppat_constraint (_, ct) -> Some ct
      | _ -> None
      end
  | Pparam_newtype _ ->
      None

let is_function ct =
  match ct.ptyp_desc with
  | Ptyp_arrow _ -> true
  | _ -> false

let get_predicate ~loc (attr_name : string) (ct : core_type) =
  let res = List.find_map (fun attr ->
    if attr.attr_name.txt = attr_name then
      match attr.attr_payload with
      (* Case: Payload is a type [PTyp] because of the ':' syntax *)
      | PTyp { ptyp_desc = Ptyp_constr (lid, _); _ } ->
          (* Convert the Longident.t from the type back into an expression *)
          Some (Exp.ident ~loc lid)

      (* Case: Payload is a structure [PStr] (standard [@pre XXX]) *)
      | PStr [{ pstr_desc = Pstr_eval (expr, _); _ }] ->
          Some expr

      | _ -> None
    else None
  ) ct.ptyp_attributes in
  Option.value res ~default:[%expr fun _ -> true]

let mk_string_param ~loc name =
  (* The type: string *)
  let string_type = Typ.constr ~loc (Located.mk ~loc (Lident "string")) [] in
  (* The pattern: (name : string) *)
  let pat = Pat.constraint_ ~loc
              (Pat.var ~loc (Located.mk ~loc name))
              string_type in
  {
    pparam_desc = Pparam_val (Nolabel, None, pat);
    pparam_loc = loc;
  }

let rec flatten_arrow_type ct =
  match ct.ptyp_desc with
  | Ptyp_arrow (_, arg, rest) ->
      let (args, ret) = flatten_arrow_type rest in
      (arg :: args, ret)
  | _ -> ([], ct)
