(* A separate, opt-in state pass. See the comment at [rewrite] for its surface
   syntax. Each declaration group owns distinct effects and one handler. *)
open Ppxlib
open Ast_builder.Default

let ident ~loc s = pexp_ident ~loc { txt = Lident s; loc }
let payload_expr ~loc = function
  | PStr [ { pstr_desc = Pstr_eval (e, []); _ } ] -> e
  | _ -> Location.raise_errorf ~loc "expected an expression in the state extension"

let parse_structure ~loc text =
  let lexbuf = Lexing.from_string text in
  Location.init lexbuf loc.loc_start.pos_fname;
  Parse.implementation lexbuf

(* A declaration group generates one shallow handler, not a tower of state
   handlers. Its answer is a transformer over a heterogeneous nested tuple;
   in particular the Check decision and its continuation share that state. *)
let contract_declaration ~loc payload =
  let bindings = match payload with
    | PStr [ { pstr_desc = Pstr_value (Nonrecursive, (_ :: _ as vbs)); _ } ] -> vbs
    | _ -> Location.raise_errorf ~loc
        "contract_states requires let name : type = initial [and ...]"
  in
  let states = List.map (fun vb ->
    match vb.pvb_pat.ppat_desc, vb.pvb_constraint with
    | Ppat_var { txt = name; _ }, Some (Pvc_constraint { locally_abstract_univars = []; typ }) ->
        (name, typ, vb.pvb_expr)
    | _ -> Location.raise_errorf ~loc:vb.pvb_loc
        "contract_states requires let name : type = initial") bindings in
  let names = List.map (fun (name, _, _) -> name) states in
  if List.length (List.sort_uniq String.compare names) <> List.length names then
    Location.raise_errorf ~loc "duplicate contract state name";
  let ctor name = "Conteff_contract_state_" ^ name in
  let gen name suffix = "__conteff_contract_state_" ^ name ^ "_" ^ suffix in
  let types = List.map (fun (_, typ, _) -> Format.asprintf "%a" Pprintast.core_type typ) states in
  let nest xs = List.fold_right (fun x tail -> "(" ^ x ^ ", " ^ tail ^ ")") xs "()" in
  let vars = List.mapi (fun i _ -> "__state" ^ string_of_int i) states in
  let state_type = List.fold_right (fun x tail -> "(" ^ x ^ " * " ^ tail ^ ")") types "unit" and state_pattern = nest vars in
  let decls = List.map2 (fun (name, _, _) ty ->
    Printf.sprintf
      "type _ Effect.t += %s_Get : (%s) Effect.t | %s_Set : (%s) -> unit Effect.t\nlet %s () = Effect.perform %s_Get\nlet %s value = Effect.perform (%s_Set value)\n"
      (ctor name) ty (ctor name) ty (gen name "get") (ctor name)
      (gen name "set") (ctor name)) states types |> String.concat "" in
  let branches = List.mapi (fun i (name, _, _) ->
    let current = List.nth vars i in
    let get_pattern = state_pattern in
    let set_pattern = nest (List.mapi (fun j v -> if i = j then "_" else v) vars) in
    let updated = nest (List.mapi (fun j v -> if i = j then "next" else v) vars) in
    Printf.sprintf
      "| %s_Get -> Some (fun (k : (b, a) continuation) ((%s) : %s) -> continue_with k %s (handler ()) %s)\n| %s_Set next -> Some (fun (k : (b, a) continuation) ((%s) : %s) -> continue_with k () (handler ()) %s)\n"
      (ctor name) get_pattern state_type current state_pattern
      (ctor name) set_pattern state_type updated) states |> String.concat "" in
  let initial_names = List.mapi (fun i _ -> "__conteff_contract_initial_" ^ string_of_int i) states in
  let initial_lets = List.mapi (fun i name ->
    Printf.sprintf "let %s = __conteff_contract_initial_placeholder_%d in\n" name i) initial_names |> String.concat "" in
  let source = Printf.sprintf {|%s
let __conteff_contract_states_run thunk =
  %s
  let open Effect.Shallow in
  let rec handler : type a. unit -> (a, %s -> a * %s) handler = fun () ->
    { retc = (fun value state -> (value, state));
      exnc = (fun e _state -> raise e);
      effc = (fun (type b) (eff : b Effect.t) ->
        match eff with
        %s
        | Conteff_lib.Contract.Check { pos; loc; pred; value } ->
            Some (fun (k : (b, a) continuation) (state : %s) ->
              let bare = { retc = Fun.id; exnc = raise; effc = (fun _ -> None) } in
              continue_with
                (fiber (fun () ->
                  match pred value with
                  | true -> continue_with k value bare
                  | false -> discontinue_with k
                      (Conteff_lib.Contract.Blame { party = pos; contract = loc }) bare
                  | exception e -> discontinue_with k e bare))
                () (handler ()) state)
        | _ -> None) } in
  fst (continue_with (fiber thunk) () (handler ()) %s)
|} decls initial_lets state_type state_type branches state_type (nest initial_names) in
  let replacements = List.mapi (fun i (_, _, expr) ->
    ("__conteff_contract_initial_placeholder_" ^ string_of_int i, expr)) states in
  let replace = object
    inherit Ast_traverse.map as super
    method! expression e =
      match e.pexp_desc with
      | Pexp_ident { txt = Lident name; _ } when List.mem_assoc name replacements -> List.assoc name replacements
      | _ -> super#expression e
  end in
  replace#structure (parse_structure ~loc source)

let mapper = object (self)
  inherit Ast_traverse.map as super
  method! structure items =
    List.concat_map (fun item ->
      match item.pstr_desc with
      | Pstr_extension (({ txt = "contract_states"; _ }, payload), []) ->
          List.map self#structure_item (contract_declaration ~loc:item.pstr_loc payload)
      | _ -> [ self#structure_item item ]) items
  method! expression e =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    | Pexp_apply ({ pexp_desc = Pexp_extension ({ txt = "contract_main_states"; _ }, PStr []); _ }, [Nolabel, body]) ->
        pexp_apply ~loc (ident ~loc "__conteff_contract_states_run")
          [Nolabel, pexp_fun ~loc Nolabel None (punit ~loc) (self#expression body)]
    | Pexp_extension ({ txt = "contract_main_states"; _ }, _) ->
        Location.raise_errorf ~loc "[%%contract_main_states] must be applied to a body"
    | Pexp_extension ({ txt = op; _ }, payload)
      when String.starts_with ~prefix:"contract_get_" op || String.starts_with ~prefix:"contract_set_" op ->
        let is_get = String.starts_with ~prefix:"contract_get_" op in
        let prefix = if is_get then "contract_get_" else "contract_set_" in
        let name = String.sub op (String.length prefix) (String.length op - String.length prefix) in
        if name = "" then Location.raise_errorf ~loc "missing contract state name";
        let arg = if is_get then (match payload with
          | PStr [] -> eunit ~loc
          | _ -> Location.raise_errorf ~loc "[%%%s] does not take a payload" op)
          else self#expression (payload_expr ~loc payload) in
        pexp_apply ~loc (ident ~loc ("__conteff_contract_state_" ^ name ^ (if is_get then "_get" else "_set"))) [Nolabel, arg]
    | _ -> super#expression e
end

(* Rewrite named contract states before the regular contract pass. *)
let rewrite structure = mapper#structure structure
