type error =
  | InvalidNumber of string
  | InvalidName of string
  | InvalidExpr of Sexplib.Sexp.t
  | InvalidStmt of Sexplib.Sexp.t
  | InvalidDecl of Sexplib.Sexp.t
  | InvalidBlock of Sexplib.Sexp.t
  | MissingFinalExpr
  | EndOfStream
  | EmptyBlock

type _ Effect.t += ParseError : error -> 'a Effect.t

module Internal : sig
  val keywords : string list
  val check_name : string -> unit

  type _ Effect.t +=
    | Next  : Sexplib.Sexp.t Effect.t
    | Peek  : Sexplib.Sexp.t option Effect.t
    | Scope : Sexplib.Sexp.t * (unit -> 'a) -> 'a Effect.t

  val parse_fields : unit -> Ast.fieldname list
  val parse_method : unit -> Ast.classmethod
  val parse_class : unit -> Ast.classdef
  val parse_stmt : unit -> Ast.statement
  val parse_expr : unit -> Ast.expression
  val parse_decl : unit -> Ast.declaration
  val parse_module : unit -> Ast.mixmodule
  val parse_typed_module : unit -> Ast.mixmodule
  val parse_mixed_system : unit -> Ast.t

  val stream_handler : (unit -> 'a) -> Sexplib.Sexp.t list -> 'a
end


val parse_mixed_system : Sexplib.Sexp.t -> Ast.t
val run : (Sexplib.Sexp.t -> Ast.t) -> Sexplib.Sexp.t -> Ast.t
