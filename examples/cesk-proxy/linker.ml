open Ast
open Mapper

open Effect
open Effect.Deep

type _ Effect.t +=
  | SynthesisResult : mixmodule list -> unit Effect.t

module QualifyName = struct

type _ Effect.t +=
  | RenameQualified : classname -> classname Effect.t

module Config = struct
  include Mapper.DefaultMapper
  let map_classname c =
      Effect.perform (RenameQualified c)

  let enter_scope = function
    | ScopeModule m -> Effect.perform (EnterModule m)
    | ScopeImport i -> Effect.perform (EnterImport i)
    | ScopeSystem d -> Effect.perform (EnterSystem d)
    | _ -> ()
end

module QualifiedNameRoutine = MakeMapper(Config)

type state = {
  modules : mixmodule list;
  current : (name (*module name*) * classname) option;
  imports : name list;
}

let lookup_imports classname imports modules : classname =
  (* order is important *)
  let imported_modules =
    List.filter_map (fun import ->
      let module_imported = function
        | Module (name, _, _) -> name = import
        | TypedModule (name, _, _, _) -> name = import
      in
      List.find_opt module_imported modules
    ) imports
  in
  let found_module =
    let class_imported = function
      | Module (_, _, Class (name, _, _, _)) -> name = classname
      | TypedModule (_, _, Class (name, _, _, _), _) -> name = classname
    in
    List.find_opt class_imported imported_modules
  in
  match found_module with
  | Some (Module (m, _, _)) -> m ^ "." ^ classname
  | Some (TypedModule (m, _, _, _)) -> m ^ "." ^ classname
  | None ->
      (* only when linking a program,
         because a program has no module *)
      classname

let run system =
  (* build up the qualified names by go through the ast tree
     this doesn't mess up Program, because a Program doesn't have Modules/Imports
   *)
  let handler s = match_with QualifiedNameRoutine.main s
  {
    retc = (fun x state -> x);
    exnc = raise;

    effc = fun (type e) (eff : e Effect.t) ->
      (match eff with
      | EnterModule (Module (name, imports, cls) as m) -> Some (fun k state ->
          let Class (classname, _, _, _) = cls in
          let new_state = {
            modules = m :: state.modules;
            current = Some (name, classname);
            imports = [];
          } in
          continue k () new_state
        )
      | EnterModule (TypedModule (name, imports, cls, _) as m) -> Some (fun k state ->
          (* facade to make a typed module acts as a module *)
          let Class (classname, _, _, _) = cls in
          let new_state = {
            modules = m :: state.modules;
            current = Some (name, classname);
            imports = [];
          } in
          continue k () new_state
        )
      | EnterSystem (MixedSystem (modules, imports, _, _, _)) -> Some (fun k state ->
          (* facade to make a typed module acts as a module *)
          let new_state = {
            modules = modules;
            current = None;
            imports = [];
          } in
          continue k () new_state
        )
      | EnterImport (Import n) -> Some (fun k state ->
          continue k () { state with imports = n :: state.imports }
        )
      | EnterImport (TypedImport (n, _)) -> Some (fun k state ->
          continue k () { state with imports = n :: state.imports }
        )
      | RenameQualified (classname) -> Some (fun k state ->
          let qualifed_name =
             match state.current with
             | Some (m, c) when c = classname -> m ^ "." ^ classname
             | _ -> lookup_imports classname state.imports state.modules
          in
          continue k qualifed_name state
        )
      | EnterSystem (Program _) -> Some (fun k state ->
          continue k () state
        )
      | _ -> None
      : ((e, state -> 'a) continuation -> state -> 'a) option)
  }
  in
  handler system { modules = []; imports = []; current = None }
end


module Synthesis = struct
type _ Effect.t +=
  | RenameImport : miximport -> miximport Effect.t

module Config = struct
  include Mapper.DefaultMapper
  let map_import i = Effect.perform (RenameImport i)

  let enter_scope = function
    | ScopeModule m -> Effect.perform (EnterModule m)
    | ScopeSystem d -> Effect.perform (EnterSystem d)
    | _ -> ()
end

module SynthesisRoutine = MakeMapper(Config)

(* keep a currated module list to be added at the end of the routine *)
type state = {
  (* list of seen modules to reference the classes *)
  modules: mixmodule list;
  (* list of new modules to be added *)
  new_modules: mixmodule list;
  (* the current module to work with,
     None for system body
   *)
  current : name option;
}

let find_utmodule modules modulename =
  List.find (fun m ->
    match m with
    | Module (m_name, _, _) when m_name = modulename -> true
    | TypedModule (m_name, _, _, _) when m_name = modulename -> true
    | _ -> false
  ) modules

let rename_classes ast =
  let handler s = match_with SynthesisRoutine.main s
  {
    retc = (fun new_ast state -> (new_ast, state));
    exnc = raise;

    effc = fun (type e) (eff : e Effect.t) ->
      (match eff with
      | EnterModule (Module (name, imports, cls) as m) -> Some (fun k state ->
          let Class (classname, _, _, _) = cls in
          let new_state = {
            modules = m :: state.modules;
            new_modules = state.new_modules;
            current = Some name;
          }
          in
          continue k () new_state
        )
      | EnterModule (TypedModule (name, imports, cls, _) as m) -> Some (fun k state ->
          let Class (classname, _, _, _) = cls in
          let new_state = {
            modules = m :: state.modules;
            new_modules = state.new_modules;
            current = Some name;
          }
          in
          continue k () new_state
        )
      | EnterSystem (MixedSystem (modules, imports, _, _, _)) -> Some (fun k state ->
          continue k () { state with current = None; }
        )
      | RenameImport (i) -> Some (fun k state ->
          (
          match i with
          | Import _ -> continue k i state
          | TypedImport (name, shape) ->
              let current_module = match state.current with
              | Some name -> name
              | None -> "0"
              in
              let new_name = name ^ ".into." ^ current_module in

              let already_added = List.find_opt (fun m ->
                match m with
                | Module (n, _, _) when n = new_name -> true
                | TypedModule (n, _, _, _) when n = new_name -> true
                | _ -> false
              ) state.new_modules
              in

              if Option.is_some already_added
              then continue k (Import new_name) state
              else

              let orig_module = find_utmodule state.modules name in
              (
              match orig_module with
              | Module (m_name, m_imports, m_cls) ->
                let new_tmodule = TypedModule (new_name, m_imports, m_cls, shape) in
                let new_state =
                  { state with new_modules = new_tmodule :: state.new_modules; }
                in
                continue k (Import new_name) new_state

              | TypedModule _ -> (* not reachable after typechecker *)
                continue k (Import new_name) state
              )
          )
        )
      | EnterSystem (Program _) -> Some (fun k state ->
          (* should never see this *)
          continue k () state
        )
      | _ -> None
      : ((e, state -> 'a) continuation -> state -> 'a) option)
  }
  in

  (* test, remove when done testing *)
  handler ast { modules = []; new_modules = []; current = None; }

let fix_modules new_modules old_modules =
  (* because modules will look for imports to rename
     we need to fix so that their order remains

     given list with "M.into.*" and "M", place "M.into.*" as close to "M"
     as possible.

     why this works: suppose N comes before M and M needs N

     (module N) (module M (import N))

     a module M.into.X will have the same import as M (because itself is a copy)
     all M's import are valid at M or after M

     suppose that import N is fixed into N.into.Y and M needs N.into.Y
     when N.into.Y is placed near to N, M should know N.into.Y already
   *)
  let get_name = function
    | Module (n, _, _) -> n
    | TypedModule (n, _, _, _) -> n
  in
  let get_root m =
    let n = get_name m in
    match String.split_on_char '.' n with
    | root :: _ -> root
    | [] -> n
  in

  let into_map = Hashtbl.create (List.length new_modules) in
  List.iter (fun m ->
    let root = get_root m in
    let existing = try Hashtbl.find into_map root with Not_found -> [] in
    Hashtbl.replace into_map root (m :: existing)
  ) new_modules;

  (* 2. Reverse the buckets because we built them via prepending (::) *)
  Hashtbl.iter (fun k v -> Hashtbl.replace into_map k (List.rev v)) into_map;

  (* 3. Reconstruct the list by iterating over old_modules and
     inserting the corresponding "into" modules immediately after each base. *)
  let rec build acc = function
    | [] -> List.rev acc
    | m :: rest ->
        let name = get_name m in
        (* Get all "into" versions that belong to this specific module name *)
        let derivatives = try Hashtbl.find into_map name with Not_found -> [] in

        (* Mark them as "placed" by removing from table to avoid double-insertion
           if the same name appears twice in old_modules (unlikely but safe) *)
        Hashtbl.remove into_map name;

        (* Acc: [derivatives_N; ...; derivative_1; M; ...rest of previous acc] *)
        let new_acc = List.rev_append derivatives (m :: acc) in
        build new_acc rest
  in
  build [] old_modules

let run ast =
  let (renamed_imports_ast, state) = rename_classes ast in
  let { new_modules; _ } = state in
  match renamed_imports_ast with
  | Program _ -> renamed_imports_ast
  | MixedSystem (modules, imports, defs, stmts, expr) ->
      MixedSystem (fix_modules new_modules modules, imports, defs, stmts, expr)
end

let run (ast : Ast.t) : Ast.t =
  let linker_pipeline ast =
    Synthesis.run ast
    |> QualifyName.run
  in

  match linker_pipeline ast with
  | MixedSystem (modules, _, decls, stmts, expr) as p ->
      (* typed-classes pass as defined, we just add the shape the class *)
      let get_class = function
        | Module (_, _, cls) -> cls
        | TypedModule (_, _, Class (name, fields, methods, _), shape) ->
            Class (name, fields, methods, Some shape)
      in
      let classes = List.map get_class modules in
      Program (classes, decls, stmts, expr)
  | Program _ as p -> p
