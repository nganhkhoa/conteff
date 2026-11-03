type error =
  | InvalidNumber of string
  | InvalidName of string
  | InvalidExpr of Sexplib.Sexp.t
  | InvalidStmt of Sexplib.Sexp.t
  | InvalidDecl of Sexplib.Sexp.t
  | InvalidBlock of Sexplib.Sexp.t
  | MixedDeclStmt of Sexplib.Sexp.t
  | MissingFinalExpr
  | UnexpectedRest of Sexplib.Sexp.t list
  | EmptyBlock

type _ Effect.t += ParseError : error -> 'a Effect.t

val run : Sexplib.Sexp.t -> Ast.t
