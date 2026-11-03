type error =
  | UndeclareVariable of string

type _ Effect.t += ValidatorError : error -> 'a Effect.t

val run : Ast.t -> Ast.t
