open Ast

type error =
  | UndeclareClass of string
  | UndeclareVariable of string
  | UnhandledStatement of statement
  | UnhandledExpression of expression
  | UnknownModule of string
  | DuplicateModuleName
  | DuplicateClassName
  | DuplicateMethodName
  | DuplicateFieldName
  | DuplicateParameterName
  | ImportMixing
  | TypeError
  | ClassShapeError
  | InternalError
  [@@deriving show, eq]

type _ Effect.t += ValidatorError : error -> 'a Effect.t

val run : Ast.t -> Ast.t
