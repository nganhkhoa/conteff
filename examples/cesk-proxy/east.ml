open Ast

(* Describing an Extended AST (eAST) with information carrying nodes

   To use this extended node, follow the following steps
   1. define WHERE information can be attached
   2. define a lifter from AST to eAST with default information
   3. define a traversal to update the information of a node
   (Effect Handlers can be used to bring states into the traversal)
 *)
type ('a, 'b) ic = {
  node : 'a;
  info : 'b;
}
[@@deriving show, eq]

(* The design is trying to compress and refine at the same time

   typed and untyped modules are compressed into one, providing
   same signature on traversing
 *)

type 'b e_expression = ('b expr_node, 'b) ic
  [@@deriving show, eq]
and 'b e_statement = ('b stmt_node, 'b) ic
  [@@deriving show, eq]
and 'b e_block = ('b block_node, 'b) ic
  [@@deriving show, eq]
and 'b e_declaration = ('b decl_node, 'b) ic
  [@@deriving show, eq]
and 'b e_classmethod = ('b classmethod_node, 'b) ic
  [@@deriving show, eq]
and 'b e_classdef = ('b classdef_node, 'b) ic
  [@@deriving show, eq]
and 'b e_import = (miximport, 'b) ic
  [@@deriving show, eq]
and 'b e_module = ('b module_node, 'b) ic
  [@@deriving show, eq]
and 'b e_system = ('b system_node, 'b) ic
  [@@deriving show, eq]


and 'b expr_node =
  | ENum of float
  | EVar of variable
  | EAdd of variable * variable
  | EDiv of variable * variable
  | EEq of variable * variable
  | ENewClass of classname * variable list
  | EFieldAccess of variable * variable
  | EMethodCall of variable * variable * variable list
  | EIsA of variable * classname
  [@@deriving show, eq]

and 'b stmt_node =
  | EAssign of variable * 'b e_expression
  | EFieldAssign of variable * fieldname * 'b e_expression
  | EIf0 of 'b e_expression * 'b e_block * 'b e_block
  | EWhile0 of 'b e_expression * 'b e_block
  [@@deriving show, eq]

and 'b block_node =
  | ESimple of 'b e_statement
  | EComplex of 'b e_declaration list * 'b e_statement list
  [@@deriving show, eq]

and 'b decl_node = {
    var: variable;
    value: 'b e_expression;
  }
  [@@deriving show, eq]

and 'b classmethod_node = {
    name: name;
    params: parameter list;
    defs: 'b e_declaration list;
    stmts: 'b e_statement list;
    body: 'b e_expression;
  }
  [@@deriving show, eq]

and 'b classdef_node = {
    name: classname;
    fields: fieldname list;
    methods: 'b e_classmethod list;
  }
  [@@deriving show, eq]

and 'b module_node = {
    name : name;
    imports : 'b e_import list;
    cls : 'b e_classdef;
    shape : shape option;
  }
  [@@deriving show, eq]

and 'b system_node = {
    modules : 'b e_module list;
    imports : 'b e_import list;
    decls   : 'b e_declaration list;
    stmts   : 'b e_statement list;
    final   : 'b e_expression;
  }
  [@@deriving show, eq]

(* conversion from AST to eAST of Info *)
module MakeLifter (Info : sig
  type t
  val default : t
end) = struct
  let wrap n = { node = n; info = Info.default }

  let rec lift_expr = function
    | Num f -> wrap (ENum f)
    | Var v -> wrap (EVar v)
    | Add (v1, v2) -> wrap (EAdd (v1, v2))
    | Div (v1, v2) -> wrap (EDiv (v1, v2))
    | Eq (v1, v2) -> wrap (EEq (v1, v2))
    | NewClass (c, vs) -> wrap (ENewClass (c, vs))
    | FieldAccess (v1, v2) -> wrap (EFieldAccess (v1, v2))
    | MethodCall (v1, v2, vs) -> wrap (EMethodCall (v1, v2, vs))
    | IsA (v, c) -> wrap (EIsA (v, c))

  let rec lift_stmt = function
    | Assign (v, e) ->
        wrap (EAssign (v, lift_expr e))
    | FieldAssign (v, f, e) ->
        wrap (EFieldAssign (v, f, lift_expr e))
    | If0 (e, b1, b2) ->
        wrap (EIf0 (lift_expr e, lift_block b1, lift_block b2))
    | While0 (e, b) ->
        wrap (EWhile0 (lift_expr e, lift_block b))

  and lift_block = function
    | Simple s ->
        wrap (ESimple (lift_stmt s))
    | Complex (decls, stmts) ->
        wrap (EComplex (List.map lift_decl decls, List.map lift_stmt stmts))

  and lift_decl (Def (v, e)) =
    wrap {
      var = v;
      value = lift_expr e;
    }

  let lift_method (Method (n, params, decls, stmts, res)) =
    wrap {
      name = n;
      params = params;
      defs = List.map lift_decl decls;
      stmts = List.map lift_stmt stmts;
      body = lift_expr res;
    }

  let lift_class (Class (cn, fields, methods, _)) =
    wrap {
      name = cn;
      fields = fields;
      methods = List.map lift_method methods;
    }

  let lift_module = function
    | TypedModule (name, imports, cdef, shape) ->
        wrap {
          name;
          imports = List.map wrap imports;
          cls = lift_class cdef;
          shape = Some shape;
        }
    | Module (name, imports, cdef) ->
        wrap {
          name;
          imports = List.map wrap imports;
          cls = lift_class cdef;
          shape = None;
        }

  let lift = function
    | MixedSystem (modules, imports, decls, stmts, expr) ->
        wrap {
          modules = List.map lift_module modules;
          imports = List.map wrap imports;
          decls   = List.map lift_decl decls;
          stmts   = List.map lift_stmt stmts;
          final   = lift_expr expr;
        }
    | _ ->
        Printf.eprintf "Unimplemented Lift System/Program\n";
        raise Stdlib.Exit

  (* ignore System and Program at the moment *)
end

let get_class_name (cls : 'b e_classdef) = cls.node.name

let get_method_name (meth : 'b e_classmethod) = meth.node.name
