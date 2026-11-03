type error =
  | DivisionZero
  | Unimplemented

type _ Effect.t += RuntimeError : error -> 'a Effect.t

type value =
  | Number of float

type _ Effect.t += RuntimeReturn : value -> 'a Effect.t

val run : Ast.t -> unit
