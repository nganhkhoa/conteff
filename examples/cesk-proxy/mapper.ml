open Ast
open East

type scope =
  | ScopeModule of mixmodule
  | ScopeImport of miximport
  | ScopeClass of classdef
  | ScopeMethod of classmethod
  | ScopeDef of declaration
  | ScopeSystem of Ast.t

(* TODO: use enter_type and implement exit *)
type _ Effect.t +=
  (* Enter for outer-to-inner handlers *)
  | EnterModule : mixmodule -> unit Effect.t
  | EnterImport : miximport -> unit Effect.t
  | EnterClass  : classdef -> unit Effect.t
  | EnterMethod : classmethod -> unit Effect.t
  | EnterDef    : declaration -> unit Effect.t
  | EnterSystem : Ast.t -> unit Effect.t

module type MAP_CONFIG = sig
  val map_variable : variable -> variable
  val map_classname : classname -> classname
  (* these are reserved for type checking in the future *)
  val map_classfield : name -> name
  val map_methodparam : name -> name
  val map_import : miximport -> miximport

  val enter_scope : scope -> unit
end

module MakeMapper (M : MAP_CONFIG) = struct
  let rec expression = function
    | Num f -> Num f
    | Var v -> Var (M.map_variable v)
    | Add (v1, v2) -> Add (M.map_variable v1, M.map_variable v2)
    | Div (v1, v2) -> Div (M.map_variable v1, M.map_variable v2)
    | Eq (v1, v2) -> Eq (M.map_variable v1, M.map_variable v2)
    | NewClass (v, vs) -> NewClass (M.map_classname v, List.map M.map_variable vs)
    | FieldAccess (v1, v2) -> FieldAccess (M.map_variable v1, v2)
    | MethodCall (v1, v2, vs) ->
        MethodCall (M.map_variable v1, v2, List.map M.map_variable vs)
    | IsA (v1, v2) -> IsA (M.map_variable v1, M.map_classname v2)

  and statement = function
    | Assign (v, e) -> Assign (M.map_variable v, expression e)
    | FieldAssign (v, f, e) -> FieldAssign (M.map_variable v, f, expression e)
    | If0 (e, b1, b2) -> If0 (expression e, block b1, block b2)
    | While0 (e, b) -> While0 (expression e, block b)

  and block = function
    | Simple s -> Simple (statement s)
    | Complex (decls, stmts) ->
        let defs = List.map declaration decls in
        Complex (defs, List.map statement stmts)

  and declaration (Def (v, e) as d) =
    M.enter_scope (ScopeDef d);
    Def (M.map_variable v, expression e)

  and classmethod (Method (n, ps, ds, ss, e) as m) =
    M.enter_scope (ScopeMethod m);
    let params = List.map M.map_methodparam ps in
    let decls = List.map declaration ds in
    Method (n, params, decls, List.map statement ss, expression e)

  and classdef (Class (n, fs, ms, s) as c) =
    M.enter_scope (ScopeClass c);
    let name = M.map_classname n in
    let fields = List.map M.map_classfield fs in
    Class (name, fields, List.map classmethod ms, s)

  and import i =
    M.enter_scope (ScopeImport i);
    M.map_import i

  and mixmodule = function
    | Module (n, is, c) as m ->
        M.enter_scope (ScopeModule m);
        let imports = List.map import is in
        Module (n, imports, classdef c)
    | TypedModule (n, is, c, sh) as m ->
        M.enter_scope (ScopeModule m);
        let imports = List.map import is in
        TypedModule (n, imports, classdef c, sh)

  and main (ast : Ast.t) =
    match ast with
    | Program (cs, ds, ss, e) as p ->
        let classes = List.map classdef cs in
        M.enter_scope (ScopeSystem p);
        let decls = List.map declaration ds in
        Program (classes, decls, List.map statement ss, expression e)
    | MixedSystem (ms, is, ds, ss, e) as s ->
        let modules = List.map mixmodule ms in
        (* lazy, we need this to resolve imports again *)
        M.enter_scope (ScopeSystem s);
        (* because ocaml processes right to left, so we need to make sure import processed first *)
        let imports = List.map import is in
        let decls = List.map declaration ds in
        MixedSystem (modules, imports, decls, List.map statement ss, expression e)
end

module DefaultMapper = struct
  let map_variable v = v
  let map_classname c = c
  let map_classfield f = f
  let map_methodparam p = p
  let map_import i = i

  let enter_scope = ()
end

