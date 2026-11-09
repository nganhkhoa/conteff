open Ast
open East

type enter_type =
  | Enter
  | Exit

(* we need a different scope for extended AST *)


(* Generic Mapper to walk the Extended AST

   Be very generic, we can carry any information
   and convert the information types at will
 *)
module type EXTENDED_MAPPER_CONFIG = sig
  type info

  type e_scope =
    | ScopeEModule of (enter_type * info e_module)
    | ScopeEImport of (enter_type * info e_import)
    | ScopeEClass of (enter_type * info e_classdef)
    | ScopeEMethod of (enter_type * info e_classmethod)
    | ScopeEDef of (enter_type * info e_declaration)
    | ScopeESystem of (enter_type * info e_system)
    | ScopeEExpr of (enter_type * info e_expression)
    | ScopeEStmt of (enter_type * info e_statement)
    | ScopeEBlock of (enter_type * info e_block)

  type _ Effect.t +=
    (* Enter for outer-to-inner handlers
       Returns the new info type
     *)
    | EnterEModule : (enter_type * info e_module) -> info Effect.t
    | EnterEImport : (enter_type * info e_import) -> info Effect.t
    | EnterEClass  : (enter_type * info e_classdef) -> info Effect.t
    | EnterEMethod : (enter_type * info e_classmethod) -> info Effect.t
    | EnterEDef    : (enter_type * info e_declaration) -> info Effect.t
    | EnterESystem : (enter_type * info e_system) -> info Effect.t
    | EnterEExpr   : (enter_type * info e_expression) -> info Effect.t
    | EnterEStmt   : (enter_type * info e_statement) -> info Effect.t
    | EnterEBlock  : (enter_type * info e_block) -> info Effect.t


  val map_variable : variable -> variable
  val map_classname : classname -> classname
  (* these are reserved for type checking in the future *)
  val map_classfield : name -> name
  val map_methodparam : name -> name

  val enter_scope : e_scope -> info
end

module MakeExtendedMapper (M : EXTENDED_MAPPER_CONFIG) = struct
  let do_map
    (type a)
    (ic_node : (a, M.info) ic)
    (scope : enter_type -> (a, M.info) ic -> M.e_scope)
    (process : a -> a)
    =
    let mid_info = M.enter_scope (scope Enter ic_node) in
    let new_node = process ic_node.node in
    let new_ic_node = { node = new_node; info = mid_info; } in
    let out_info = M.enter_scope (scope Exit new_ic_node) in
    { new_ic_node with info = out_info }

  let rec map_expr (e : M.info e_expression) : M.info e_expression =
    do_map e (fun s n -> M.ScopeEExpr (s, n))
      @@
      function
        | ENum f -> ENum f
        | EVar v -> EVar (M.map_variable v)
        | EAdd (v1, v2) -> EAdd (M.map_variable v1, M.map_variable v2)
        | EDiv (v1, v2) -> EDiv (M.map_variable v1, M.map_variable v2)
        | EEq (v1, v2) -> EEq (M.map_variable v1, M.map_variable v2)
        | ENewClass (cn, vs) -> ENewClass (M.map_classname cn, List.map M.map_variable vs)
        | EFieldAccess (v, f) -> EFieldAccess (M.map_variable v, f)
        | EMethodCall (v, m, vs) -> EMethodCall (M.map_variable v, m, List.map M.map_variable vs)
        | EIsA (v, cn) -> EIsA (M.map_variable v, M.map_classname cn)

  and map_stmt (s : M.info e_statement) : M.info e_statement =
    do_map s (fun s n -> M.ScopeEStmt (s, n))
      @@
      function
        | EAssign (v, e) -> EAssign (M.map_variable v, map_expr e)
        | EFieldAssign (v, f, e) -> EFieldAssign (M.map_variable v, f, map_expr e)
        | EIf0 (e, b1, b2) -> EIf0 (map_expr e, map_block b1, map_block b2)
        | EWhile0 (e, b) -> EWhile0 (map_expr e, map_block b)

  and map_block (b : M.info e_block) : M.info e_block =
    do_map b (fun s n -> ScopeEBlock (s, b))
    @@
    function
      | ESimple s -> ESimple (map_stmt s)
      | EComplex (ds, ss) -> EComplex (List.map map_decl ds, List.map map_stmt ss)

  and map_decl (d : M.info e_declaration) : M.info e_declaration =
    do_map d (fun s n -> ScopeEDef (s, n))
    @@
    fun node ->
    {
      var = M.map_variable node.var;
      value = map_expr node.value;
    }

  let map_method (me : M.info e_classmethod) : M.info e_classmethod =
    do_map me (fun s n -> ScopeEMethod (s, n))
    @@
    fun { name; params; defs; stmts; body; } ->
    {
      name = name;
      params = List.map M.map_methodparam params;
      defs = List.map map_decl defs;
      stmts = List.map map_stmt stmts;
      body = map_expr body;
    }

  let map_class (c : M.info e_classdef) : M.info e_classdef =
    do_map c (fun s n -> ScopeEClass(s, n))
    @@
    fun { name; fields; methods; } ->
    {
      name = M.map_classname name;
      fields = List.map M.map_classfield fields;
       methods = List.map map_method methods;
    }

  let map_import (i : M.info e_import) : M.info e_import =
    do_map i (fun s n -> ScopeEImport (s, n))
    @@ fun i -> i

  let map_module (mo : M.info e_module) : M.info e_module =
    do_map mo (fun s n -> ScopeEModule (s, n))
    @@ fun { name; imports; cls; shape; } ->
    {
      name = name;
      imports = List.map map_import imports;
      cls = map_class cls;
      shape = shape;
    }

  let map_system (sys : M.info e_system) : M.info e_system =
    do_map sys (fun s n -> ScopeESystem (s, n))
    @@ fun { modules; imports; decls; stmts; final; } ->
    {
      modules = List.map map_module modules;
      imports = List.map map_import imports;
      decls = List.map map_decl decls;
      stmts = List.map map_stmt stmts;
      final = map_expr final;
    }
end

module DefaultExtendedMapper = struct
  type info = unit

  let map_variable v = v
  let map_classname c = c
  let map_classfield f = f
  let map_methodparam p = p

  let enter_scope = ()
end

