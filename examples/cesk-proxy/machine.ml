open Ast
open Effect
open Effect.Deep

type error =
  | DivisionZero
  | ClassInitWrongArity of string
  | ClassNotFound of string
  | FieldNotFound of string
  | MethodNotFound of string
  | MethodWrongArity of string * string
  | ExpectedNumber
  | ExpectedObject
  | NotConform
  | Unimplemented
  | SystemCannotRun
  [@@deriving show, eq]

type _ Effect.t += RuntimeError : error -> 'a Effect.t

let pp_hashtbl pp_key pp_val fmt tbl =
  Format.fprintf fmt "@[[";
  let first = ref true in
  Hashtbl.iter (fun k v ->
    if !first then first := false else Format.fprintf fmt ";@ ";
    Format.fprintf fmt "%a: %a" pp_key k pp_val v
  ) tbl;
  Format.fprintf fmt "]@]"

type oid = int

type value =
  | Number of float
  | Object of oid (*instance id*) * classname * (fieldname, value) Hashtbl.t
      [@printer fun fmt (i, n, h) ->
        Format.fprintf fmt "(%a, %a)" pp_name n (pp_hashtbl pp_fieldname pp_value) h]
  | Proxy of value * shape
  [@@deriving show]

type _ Effect.t += RuntimeReturn : value -> 'a Effect.t

module Internal = struct

(* bound the scope with class so we can access any class anywhere *)
type _ Effect.t +=
  | GetClass : string -> classdef option Effect.t
  | NewInstanceId : oid Effect.t

type current =
  | Expression of expression
  | Value of value
  | Searching
  [@@deriving show]


(* mapping string -> storage index *)
type environment = (variable, int) Hashtbl.t

(* mapping idx -> value

  Ocaml list performs best if prepend (add to head)
  index 0 -> list size - 1
  index 1 -> list size - 1 - 1
  index n -> list size -1 - n
 *)
type storage = value list

(* statement or block

   because block are also defined inside While0 and If0
   we can uniform them into block
 *)
type closure =
  | Closure of declaration list * block list * expression option
  [@@deriving show]

(* kstack stack

  popping the stack by List.tail
 *)
type stack_item =
    | Kontinuation of (closure * environment)
    | MethodReturnCheck of typ
type kstack = stack_item list

type _ Effect.t += SingleStep : (current * environment * storage * kstack) -> unit Effect.t

(*
 * Helper constants
 *)

let truthful = 0.0
let falsy = 1.0

(*
 * Helper functions
 *)

let conform (obj : value) (t : typ) =
    match (obj, t) with
    | (Number _, Real) -> Some obj
    | (Object _, Shape s) -> Some (Proxy (obj, s))
    | (Proxy (_, s1), s2) when Typechecker.type_eq (Shape s1) s2 -> Some obj
    | _ -> None

(* The function bodies perform the representation-changing part of the cast. *)
let conforms typ value = Option.is_some (conform value typ)

(* Conformance must either preserve the original value or wrap an object in
   precisely the requested shape; checking conformance alone is too weak. *)
let conformance_result typ original result =
  match typ, original, result with
  | Real, Number _, _ -> result == original
  | Shape shape, Object _, Proxy (inner, actual_shape) ->
      inner == original && Typechecker.type_eq (Shape shape) (Shape actual_shape)
  | Shape _, Proxy _, _ -> result == original && conforms typ result
  | _ -> false

let
  [@contract dep_args
    [
      (typ, Conteff_lib.Contract.any);
      (value, conforms typ)
    ]
    (fun typ value -> conformance_result typ value)
  ]
  process_conformance typ value =
  match conform value typ with
  | Some value -> value
  | None -> assert false

let arguments_conform types values =
  List.compare_lengths types values = 0 &&
  List.for_all2 conforms types values

let arguments_conformance_result types values results =
  List.compare_lengths values results = 0 &&
  List.for_all2 (fun (typ, value) result ->
    conformance_result typ value result)
    (List.combine types values) results

let
  [@contract dep_args
    [
      (types, Conteff_lib.Contract.any);
      (values, arguments_conform types)
    ]
    (fun types values -> arguments_conformance_result types values)
  ]
  process_arguments types values =
  List.map2 process_conformance types values

(* Locations count from the oldest storage value.  Inspect them without
   raising so invalid bindings are rejected at the relevant boundary. *)
let stored_value env sto var =
  match Hashtbl.find_opt env var with
  | Some idx when idx >= 0 && idx < List.length sto ->
      List.nth_opt sto (List.length sto - 1 - idx)
  | _ -> None

let has_stored_value env sto var =
  Option.is_some (stored_value env sto var)

let stored_value_result env sto var result =
  match stored_value env sto var with
  | Some value -> result == value
  | None -> false

let
  [@contract dep_args
    [
      (env, Conteff_lib.Contract.any);
      (sto, Conteff_lib.Contract.any);
      (var, has_stored_value env sto)
    ]
    (fun env sto var -> stored_value_result env sto var)
  ]
  fetch_value env sto var =
  let idx = Hashtbl.find env var in
  List.nth sto (List.length sto - 1 - idx)

let has_stored_number env sto var =
  match stored_value env sto var with
  | Some (Number _) -> true
  | _ -> false

let stored_number_result env sto var result =
  match stored_value env sto var with
  | Some (Number n) -> Float.equal result n
  | _ -> false

let
  [@contract dep_args
    [
      (env, Conteff_lib.Contract.any);
      (sto, Conteff_lib.Contract.any);
      (var, has_stored_number env sto)
    ]
    (fun env sto var -> stored_number_result env sto var)
  ]
  fetch_number env sto var =
  match fetch_value env sto var with
  | Number n -> n
  | _ -> assert false

(* Normalize only objects and one-layer object proxies, as before.  The
   dependent result contract also checks that no field table is copied. *)
let is_object_or_proxy = function
  | Object _ | Proxy (Object _, _) -> true
  | _ -> false

let object_parts_result value (name, fields, shape) =
  match value, shape with
  | Object (_, expected_name, expected_fields), None ->
      name = expected_name && fields == expected_fields
  | Proxy (Object (_, expected_name, expected_fields), expected_shape), Some actual_shape ->
      name = expected_name && fields == expected_fields && actual_shape == expected_shape
  | _ -> false

let
  [@contract dep_args
    [
      (value, is_object_or_proxy)
    ]
    (fun value -> object_parts_result value)
  ]
  object_parts value =
  match value with
    | Object (_, name, fields) -> (name, fields, None)
    | Proxy (Object (_, name, fields), shape) -> (name, fields, Some shape)
    | _ -> assert false

(* Check the receiver at this boundary, not only inside object_parts. *)
let has_stored_object env sto var =
  match stored_value env sto var with
  | Some (Object _) | Some (Proxy (Object _, _)) -> true
  | _ -> false

let stored_object_result env sto var (name, fields, shape) =
  match stored_value env sto var, shape with
  | Some (Object (_, expected_name, expected_fields)), None ->
      name = expected_name && fields == expected_fields
  | Some (Proxy (Object (_, expected_name, expected_fields), expected_shape)), Some actual_shape ->
      name = expected_name && fields == expected_fields && actual_shape == expected_shape
  | _ -> false

let
  [@contract dep_args
    [
      (env, Conteff_lib.Contract.any);
      (sto, Conteff_lib.Contract.any);
      (var, has_stored_object env sto)
    ]
    (fun env sto var -> stored_object_result env sto var)
  ]
  fetch_object env sto var =
  object_parts (fetch_value env sto var)

let try_fetch_object env sto var =
  let idx = Hashtbl.find env var in
  match List.nth sto (List.length sto - 1 - idx) with
  | Object (_, name, fields) -> Some (name, fields)
  | Proxy (Object (_, clsname, fields), shape) -> Some (clsname, fields)
  | _ -> None

(* Class lookup is an effect, so both the precondition and postcondition
   consult the active class handler.  The result is the first declaration. *)
let lookup_method classname methodname =
  match perform (GetClass classname) with
  | Some (Class (_, _, methods, _)) ->
      List.find_opt (fun (Method (name, _, _, _, _)) -> name = methodname) methods
  | None -> None

let class_has_method classdef methodname =
  match classdef with
  | Some (Class (_, _, methods, _)) ->
      List.exists (fun (Method (name, _, _, _, _)) -> name = methodname) methods
  | None -> false

let is_expected_result expected result =
  match expected with
  | Some expected -> result == expected
  | None -> false

let
  [@contract dep_args
    [
      (classname, Conteff_lib.Contract.any);
      (methodname, let classdef = perform (GetClass classname) in
        class_has_method classdef)
    ]
    (fun classname methodname ->
      let expected = lookup_method classname methodname in
      is_expected_result expected)
  ]
  fetch_method classname methodname =
  match perform (GetClass classname) with
  | Some (Class (_, _, methods, _)) ->
      List.find (fun (Method (name, _, _, _, _)) -> name = methodname) methods
  | None -> assert false

(* The declared method must exist on the proxy shape; return its first entry,
   preserving the same lookup behavior as runtime method lookup. *)
let has_tmethod methodname tmethods =
  List.exists (fun (name, _, _) -> name = methodname) tmethods

let
  [@contract dep_args
    [
      (methodname, Conteff_lib.Contract.any);
      (tmethods, has_tmethod methodname)
    ]
    (fun methodname tmethods ->
      let expected = List.find_opt (fun (name, _, _) -> name = methodname) tmethods in
      is_expected_result expected)
  ]
  get_tmethod methodname tmethods =
  List.find (fun (name, _, _) -> name = methodname) tmethods

(* These monitors describe the table operations independently of proxy casts.
   A read returns the actual stored value; a write installs exactly the value
   supplied, without replacing the table or introducing a missing field. *)
let field_read_matches fields field result =
  match Hashtbl.find_opt fields field with
  | Some expected -> result == expected
  | None -> false

let field_write_matches fields field value () =
  match Hashtbl.find_opt fields field with
  | Some stored -> stored == value
  | None -> false

let
  [@contract dep_args
    [
      (fields, Conteff_lib.Contract.any);
      (field, Hashtbl.mem fields)
    ]
    (fun fields field -> field_read_matches fields field)
  ]
  read_existing_field fields field =
  Hashtbl.find fields field

let
  [@contract dep_args
    [
      (fields, Conteff_lib.Contract.any);
      (field, Hashtbl.mem fields);
      (value, Conteff_lib.Contract.any)
    ]
    (fun fields field value -> field_write_matches fields field value)
  ]
  write_existing_field fields field value =
  Hashtbl.replace fields field value

(* Inspect access arguments without raising: invalid environment bindings,
   non-objects, absent fields and fields hidden by a proxy must fail the
   boundary contract before the operation is entered. *)
let field_object env sto obj =
  match Hashtbl.find_opt env obj with
  | Some idx when idx >= 0 && idx < List.length sto ->
      begin match List.nth_opt sto (List.length sto - 1 - idx) with
      | Some (Object (_, _, fields)) -> Some (fields, None)
      | Some (Proxy (Object (_, _, fields), shape)) -> Some (fields, Some shape)
      | _ -> None
      end
  | _ -> None

let accessible_field env sto obj field =
  match field_object env sto obj with
  | Some (fields, None) when Hashtbl.mem fields field -> Some (fields, None)
  | Some (fields, Some (tfields, _)) when Hashtbl.mem fields field ->
      Option.map (fun typ -> (fields, Some typ))
        (List.find_map (fun (name, typ) ->
          if name = field then Some typ else None) tfields)
  | _ -> None

(* Build the read predicate before executing the operation, so it compares
   against the value that was present at entry, not a later table lookup. *)
let field_read_result env sto obj field =
  let entry = Option.bind (accessible_field env sto obj field)
      (fun (fields, typ) ->
        Option.map (fun value -> (fields, typ, value)) (Hashtbl.find_opt fields field)) in
  fun result ->
    match entry with
    | Some (fields, typ, original) ->
        (match Hashtbl.find_opt fields field with
         | Some current -> current == original | None -> false) &&
        (match typ with
         | None -> result == original
         | Some typ -> conformance_result typ original result)
    | None -> false

let field_write_result env sto obj field value =
  let entry = accessible_field env sto obj field in
  fun () ->
    match entry with
    | Some (fields, typ) ->
        begin match Hashtbl.find_opt fields field with
        | Some stored ->
            (match typ with
             | None -> stored == value
             | Some typ -> conformance_result typ value stored)
        | None -> false
        end
    | None -> false

let
  [@contract dep_args
    [
      (env, Conteff_lib.Contract.any);
      (sto, Conteff_lib.Contract.any);
      (obj, fun obj -> Option.is_some (field_object env sto obj));
      (field, fun field -> Option.is_some (accessible_field env sto obj field))
    ]
    (fun env sto obj field -> field_read_result env sto obj field)
  ]
  get_field_value env sto obj field =
  (* The boundary has already established both the runtime field and, for a
     proxy, its declared type.  Do not repeat those checks here. *)
  let fields, typ = Option.get (accessible_field env sto obj field) in
  let value = read_existing_field fields field in
  match typ with
  | None -> value
  | Some typ -> process_conformance typ value

let
  [@contract dep_args
    [
      (env, Conteff_lib.Contract.any);
      (sto, Conteff_lib.Contract.any);
      (obj, fun obj -> Option.is_some (field_object env sto obj));
      (field, fun field -> Option.is_some (accessible_field env sto obj field));
      (value, Conteff_lib.Contract.any)
    ]
    (fun env sto obj field value -> field_write_result env sto obj field value)
  ]
  update_field_value env sto obj field new_value =
  let fields, typ = Option.get (accessible_field env sto obj field) in
  let value = match typ with
    | None -> new_value
    | Some typ -> process_conformance typ new_value
  in
  write_existing_field fields field value

(* Construct exactly the common positional prefix.  The typed-import case can
   intentionally have more real fields or more declared fields than arguments;
   ordinary constructors check equal arity before reaching this helper. *)
let rec field_bindings fields values =
  match fields, values with
  | field :: fields, value :: values ->
      (field, value) :: field_bindings fields values
  | _, _ -> []

let table_matches_bindings bindings table =
  Hashtbl.length table = List.length bindings &&
  List.for_all (fun (field, _) ->
    let expected = List.fold_left (fun found (key, value) ->
      if key = field then value :: found else found) [] bindings in
    let actual = Hashtbl.find_all table field in
    List.compare_lengths expected actual = 0 &&
    List.for_all2 ( == ) expected actual) bindings

let
  [@contract dep_args
    [
      (fields, Conteff_lib.Contract.any);
      (values, Conteff_lib.Contract.any)
    ]
    (fun fields values ->
      let bindings = field_bindings fields values in
      fun table -> table_matches_bindings bindings table)
  ]
  make_field_table fields values =
  let table = Hashtbl.create (min (List.length fields) (List.length values)) in
  List.iter (fun (field, value) -> Hashtbl.add table field value)
    (field_bindings fields values);
  table

(* The result must preserve the requested class, positional field values, and
   exact imported shape, not merely be some object or proxy. *)
let object_result classname bindings shape result =
  match shape, result with
  | None, Object (_, name, table) ->
      name = classname && table_matches_bindings bindings table
  | Some expected, Proxy (Object (_, name, table), actual) ->
      name = classname && actual == expected &&
      table_matches_bindings bindings table
  | _ -> false

let
  [@contract dep_args
    [
      (classname, Conteff_lib.Contract.any);
      (fields, Conteff_lib.Contract.any);
      (values, Conteff_lib.Contract.any);
      (shape, Conteff_lib.Contract.any)
    ]
    (fun classname fields values shape ->
      let bindings = field_bindings fields values in
      object_result classname bindings shape)
  ]
  make_object classname fields values shape =
  let table = make_field_table fields values in
  let id = perform NewInstanceId in
  let obj = Object (id, classname, table) in
  match shape with None -> obj | Some shape -> Proxy (obj, shape)

(* Compare only the shared prefix: imported shapes may declare more or fewer
   fields than the implementation, but their common positions must agree. *)
let rec constructor_fields_aligned fields tfields =
  match fields, tfields with
  | field :: fields, (name, _) :: tfields ->
      field = name && constructor_fields_aligned fields tfields
  | _, _ -> true

(* Resolve the class while constructing the dependent contract, not inside its
   predicate.  The Check handler may run predicates outside with_classes, where
   GetClass would be unhandled.  Typed argument arity and conformance remain
   the responsibility of process_arguments, as before. *)
let valid_constructor_arguments classdef args =
  match classdef with
  | None -> false
  | Some (Class (_, fields, _, None)) ->
      List.compare_lengths fields args = 0
  | Some (Class (_, fields, _, Some (tfields, _))) ->
      constructor_fields_aligned fields tfields

let
  [@contract dep_args
    [
      (classname, Conteff_lib.Contract.any);
      (args, let classdef = perform (GetClass classname) in
        valid_constructor_arguments classdef)
    ]
    (fun classname args -> Conteff_lib.Contract.any)
  ]
  new_object (classname : name) (args : value list) =
  match perform (GetClass classname) with
  | Some (Class (_, fields, _, None)) ->
      make_object classname fields args None
  | Some (Class (_, fields, _, Some ((tfields, _) as shape))) ->
      let args = process_arguments (List.map snd tfields) args in
      make_object classname fields args (Some shape)
  | None -> assert false

(* allocate new storage locations
   maps them to values
   and maps environment names -> new location
 *)
let
  [@contract dep_args
    [
      (_env, Conteff_lib.Contract.any);
      (_sto, Conteff_lib.Contract.any);
      (names, Conteff_lib.Contract.any);
      (_values, fun values -> List.compare_lengths names values = 0)
    ]
    (fun _env _sto names _values -> Conteff_lib.Contract.any)
  ]
  assign_new_values env sto names values =
  List.fold_left2 (fun acc_sto name value ->
    let new_idx = List.length acc_sto in
    Hashtbl.replace env name new_idx;
    value :: acc_sto
  ) sto names values

let rename_expression expr =
  let mapping = Hashtbl.create 10 in
  let re = Str.regexp "\\(.*\\)\\$\\([0-9]+\\)$" in
  let rename var =
    match Hashtbl.find_opt mapping var with
    | Some new_name -> new_name
    | None ->
        let new_name =
          if Str.string_match re var 0 then
            let base = Str.matched_group 1 var in
            let index = int_of_string (Str.matched_group 2 var) in
            Printf.sprintf "%s$%d" base (index + 1)
          else
            var ^ "$0"
        in
        Hashtbl.replace mapping var new_name;
        new_name
  in

  let new_expr =
    match expr with
    | Num f -> Num f
    | Var v -> Var (rename v)
    | Add (v1, v2) -> Add (rename v1, rename v2)
    | Div (v1, v2) -> Div (rename v1, rename v2)
    | Eq (v1, v2) -> Eq (rename v1, rename v2)
    | NewClass (cls, v_list) -> NewClass (cls, List.map rename v_list)
    | FieldAccess (v1, f) -> FieldAccess (rename v1, f)
    | MethodCall (v1, m, v_list) -> MethodCall (rename v1, m, List.map rename v_list)
    | IsA (v1, cls) -> IsA (rename v1, cls)
  in
  (mapping, new_expr)

let rec compare_values (env : environment) (sto : storage) (v1 : value) (v2 : value) (traversed : (oid * oid) list) =
  match (v1, v2) with
  | (Number n1, Number n2) ->
    let float_equal ?(epsilon = 1e-9) a b =
        Float.abs (a -. b) < epsilon
    in
    if float_equal n1 n2
    then truthful
    else falsy
  | (Object (id1, o1name, o1fields), Object (id2, o2name, o2fields)) when id1 = id2 ->
      truthful
  | (Object (id1, o1name, o1fields), Object (id2, o2name, o2fields)) when o1name = o2name ->
      if (List.mem (id1, id2) traversed) || (List.mem (id2, id1) traversed)
      then truthful
      else if Hashtbl.length o1fields <> Hashtbl.length o2fields then falsy
      else
          Hashtbl.fold (fun field v1 acc ->
            match Hashtbl.find_opt o2fields field with
            | None -> falsy
            | Some v2 ->
                if compare_values env sto v1 v2 ((id1, id2) :: traversed) = falsy
                then falsy
                else acc
          ) o1fields truthful
  | (Proxy (o1, _), Proxy (o2, _)) -> compare_values env sto o1 o2 traversed
  | (Proxy (o1, _), Object _) -> compare_values env sto o1 v2 traversed
  | (Object _, Proxy (o2, _)) -> compare_values env sto v1 o2 traversed
  | _ -> falsy

(*
 * Machine step
 *)

(* A state is checked immediately before every transition, including recursive
   calls. Storage locations count from the oldest value, not the list head.
   Saved environments can be used after a scope or method returns, so they
   must be valid against the same (monotonically allocated) storage too. *)
let valid_step_state (c, e, s, k) =
  let length = List.length s in
  let valid_env env =
    Hashtbl.fold (fun _ loc valid -> valid && loc >= 0 && loc < length) env true
  in
  let bound name = Hashtbl.mem e name in
  let expression_bound = function
    | Num _ -> true
    | Var v | IsA (v, _) | FieldAccess (v, _) -> bound v
    | Add (v1, v2) | Div (v1, v2) | Eq (v1, v2) ->
        bound v1 && bound v2
    | NewClass (_, args) -> List.for_all bound args
    | MethodCall (receiver, _, args) -> bound receiver && List.for_all bound args
  in
  valid_env e &&
  List.for_all (function
    | Kontinuation (_, saved) -> valid_env saved
    | MethodReturnCheck _ -> true) k &&
  (match c, k with
   | Expression expr, _ -> expression_bound expr
   | Value value, MethodReturnCheck typ :: _ -> conforms typ value
   (* These branches use the variable directly, without first stepping to an
      Expression. Do not inspect future statements in a closure: declarations
      and nested scopes may introduce their variables later. *)
   | Value _, Kontinuation (Closure ([], Simple (Assign (v, _)) :: _, _), _) :: _
   | Value _, Kontinuation (Closure ([], Simple (FieldAssign (v, _, _)) :: _, _), _) :: _ -> bound v
   | Searching, Kontinuation (Closure ([], [], Some expr), _) :: _ ->
       expression_bound expr
   | _ -> true)

(* The last argument checks the complete state, including recursive calls.
   Execution normally exits by effect, so the result contract is unconstrained.
   The enclosing runner must supply the contract Check handler. *)
let
  [@contract dep_args
    [
      (c, Conteff_lib.Contract.any);
      (e, Conteff_lib.Contract.any);
      (s, Conteff_lib.Contract.any)
    ]
    (fun c e s -> (fun k -> valid_step_state (c, e, s, k)) @-> Conteff_lib.Contract.any)
  ]
  rec step (c : current) (e : environment) (s : storage) (k : kstack) =
  (* trick to stop one step, for testing or debug *)
  let _ = Effect.perform (SingleStep (c, e, s, k)) in

  match (c, k) with
  (* done, nothing else to evaluate *)
  | Value v, [] -> Effect.perform (RuntimeReturn v)

  (* expressions *)
  | Expression Num n, _ -> step (Value (Number n)) e s k
  | Expression Var var, _ ->
      let v = fetch_value e s var in
      step (Value v) e s k
  | Expression Add (lhs, rhs), _ ->
      let v1 = fetch_number e s lhs in
      let v2 = fetch_number e s rhs in
      step (Value (Number (v1 +. v2))) e s k
  | Expression Div (lhs, rhs), _ ->
      let v1 = fetch_number e s lhs in
      let v2 = fetch_number e s rhs in
      if v2 = 0.0 then assert false;
      step (Value (Number (v1 /. v2))) e s k
  | Expression Eq (lhs, rhs), _ ->
      let v1 = fetch_value e s lhs in
      let v2 = fetch_value e s rhs in
      let r = (compare_values e s v1 v2 []) in
      step (Value (Number r)) e s k

  (* blocks and definition *)

  | Searching, (Kontinuation (Closure ((Def (_, expr)) :: _, _, _), _) :: _) ->
      step (Expression expr) e s k

  | Value v, (Kontinuation (Closure ((Def (var, _)) :: defs, stmts, expr), env) :: krest) ->
      let closure = Closure (defs, stmts, expr) in
      let idx = List.length s in
      Hashtbl.replace e var idx;
      step Searching e (v :: s) (Kontinuation (closure, env) :: krest)

  (* note: search encounters nested block *)
  | Searching, (Kontinuation (Closure ([], Complex (defs, stmts) :: rstmt, expr), env) :: krest) ->
      let nested_stmts = List.map (fun s -> Simple s) stmts in
      let cl = Closure (defs, nested_stmts, None) in
      let k = Closure ([], rstmt, expr) in
      (* make a new scope by cloning the current mappings *)
      step Searching (Hashtbl.copy e) s (Kontinuation (cl, e) :: Kontinuation (k, env) :: krest)

  | Searching, (Kontinuation (Closure ([], [], None), old_e) :: krest) ->
      step Searching old_e s krest

  (* PITCH *)
  | Searching, (Kontinuation (Closure ([], [], (Some r)), old_e) :: krest) ->
      (* Printf.eprintf "k size = %d\n" (List.length k); *)
      let (mapping, r_renamed) = rename_expression r in

      let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) mapping [] in
      let old_names = List.map fst pairs in
      let new_names = List.map snd pairs in

      let old_values = List.map (fetch_value e s) old_names in
      let new_sto = assign_new_values old_e s new_names old_values in

      step (Expression r_renamed) old_e new_sto krest

  | Value _, (Kontinuation (Closure ([], [], (Some _)), old_e) :: krest) -> step c old_e s krest

  (* assignment *)

  | Searching, (Kontinuation (Closure ([], Simple (Assign (_, rhs)) :: rstmt, _), _) :: krest) ->
      step (Expression rhs) e s k
  | Value v, (Kontinuation (Closure ([], Simple (Assign (var, _)) :: rstmt, expr), env) :: krest) ->
      let idx = Hashtbl.find e var in
      let new_s =
        List.mapi (fun i old_v -> if i = (List.length s - 1 - idx) then v else old_v) s
      in
      let closure = Closure ([], rstmt, expr) in
      step Searching e new_s (Kontinuation (closure, env) :: krest)

  (* while loops *)

  | Searching, (Kontinuation (Closure ([], Simple (While0 (tst, body)) :: _, _), _) :: krest) ->
      step (Expression tst) e s k
  | Value (Number 0.), (Kontinuation (Closure ([], (Simple (While0 (tst, body)) as while_stmt) :: rstmt, expr), env) :: krest) ->
      let closure = Closure ([], (body :: while_stmt :: rstmt), expr) in
      step Searching e s (Kontinuation (closure, env) :: krest)
  | Value _, (Kontinuation (Closure ([], Simple (While0 (tst, body)) :: rstmt, expr), env) :: krest) ->
      let closure = Closure ([], rstmt, expr) in
      step Searching e s (Kontinuation (closure, env) :: krest)

  (* conditionals *)

  | Searching, (Kontinuation (Closure ([], Simple (If0 (tst, thn, els)) :: rstmt, expr), env) :: krest) ->
      step (Expression tst) e s k
  | Value (Number 0.), (Kontinuation (Closure ([], Simple (If0 (tst, thn, els)) :: rstmt, expr), env) :: krest) ->
      let closure = Closure ([], thn :: rstmt, expr) in
      step Searching e s (Kontinuation (closure, env) :: krest)
  | Value _, (Kontinuation (Closure ([], Simple (If0 (tst, thn, els)) :: rstmt, expr), env) :: krest) ->
      let closure = Closure ([], els :: rstmt, expr) in
      step Searching e s (Kontinuation (closure, env) :: krest)

  (* class related *)
  | Expression (NewClass (classname, args)), k ->
      let arg_values = List.map (fetch_value e s) args in
      let obj = new_object classname arg_values in
      step (Value obj) e s k

  | Expression (IsA (o, classname)), k ->
      (
      match try_fetch_object e s o with
      | Some (cls, fields) ->
          if cls = classname then
            step (Value (Number truthful)) e s k
          else step (Value (Number falsy)) e s k
      | _ -> step (Value (Number falsy)) e s k
      )

  | Expression (FieldAccess (o, fieldname)), k ->
      step (Value (get_field_value e s o fieldname)) e s k

  | Searching, (Kontinuation (Closure ([], Simple (FieldAssign (o, fieldname, rhs)) :: rstmt, expr), env) :: krest) ->
      step (Expression rhs) e s k

  | Value v, (Kontinuation (Closure ([], Simple (FieldAssign (o, fieldname, rhs)) :: rstmt, expr), env) :: krest) ->
      let _ = update_field_value e s o fieldname v in
      let new_k = (Kontinuation (Closure ([], rstmt, expr), env) :: krest) in
      step Searching e s new_k

  | Expression (MethodCall (o, m, args)), k ->
      let (cls, fields, shape) = fetch_object e s o in
      begin
      match shape with
      | None ->
          let Method (_, params, decls, stmts, expr) = fetch_method cls m in
          let arg_values = List.map (fetch_value e s) (o :: args) in
          let new_env = Hashtbl.create 0 in
          let new_sto = assign_new_values new_env s ("this" :: params) arg_values in
          let closure = Closure (decls, [Complex ([], stmts)], Some expr) in
          step Searching new_env new_sto (Kontinuation (closure, e) :: k)

      | Some (_, tmethods) ->
          let Method (_, params, decls, stmts, expr) = fetch_method cls m in
          let arg_values = List.map (fetch_value e s) args in
          let this_value = fetch_value e s o in

          let (_, t_params, t_ret) = get_tmethod m tmethods in

          let new_arg_values = process_arguments t_params arg_values in

          let new_env = Hashtbl.create 0 in
          let new_sto = assign_new_values new_env s ("this" :: params) (this_value :: new_arg_values) in
          let closure = Closure (decls, [Complex ([], stmts)], Some expr) in
          let ret_check = MethodReturnCheck t_ret in
          step Searching new_env new_sto (Kontinuation (closure, e) :: ret_check :: k)
      end

  | Value v, (MethodReturnCheck typ :: krest) ->
      step (Value (Option.get (conform v typ))) e s krest

  | _ -> assert false

let with_classes classes f =
  let rec find_class (classes : classdef list) (classname : string) =
    match classes with
    | [] -> None
    | Class (name, _, _, _) as hd :: tail ->
        if classname = name then
            Some hd
        else
            find_class tail classname
  in

  let instance_id : oid ref = ref 0 in

  Effect.Deep.try_with f ()
  { effc = fun (type e) (eff : e Effect.t) ->
    (match eff with
    | GetClass classname -> Some(fun k ->
      (continue k (find_class classes classname))
    )
    | NewInstanceId -> Some(fun k ->
      let id = !instance_id in
      instance_id := (!instance_id) + 1;
      (continue k id)
    )
    | _ -> None

    : ((e, unit) continuation -> unit) option)
  }
end

(* Machine runner runs the program according to CESK machine *)
let run = function
  | MixedSystem _ -> Effect.perform (RuntimeError SystemCannotRun)
  | Program (classes, defs, stmts, expr) as p ->
    let env = Hashtbl.create 0 in
    let closure = Internal.Closure (defs, List.map (fun s -> Simple s) stmts, Some expr) in
    let k = [Internal.Kontinuation (closure, env)] in

    Internal.with_classes classes (fun () -> Internal.step Searching env [] k)
