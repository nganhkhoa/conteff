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

type oid = int

type value =
  | Number of float
  | Object of oid (*instance id*) * Ast.name * (Ast.fieldname, value) Hashtbl.t
  | Proxy of value * Ast.shape
  [@@deriving show]

type _ Effect.t += RuntimeReturn : value -> 'a Effect.t

module Internal : sig
  type _ Effect.t +=
    | GetClass : string -> Ast.classdef option Effect.t
    | NewInstanceId : oid Effect.t

  type current =
    | Expression of Ast.expression
    | Value of value
    | Searching
    [@@deriving show]

  type environment = (Ast.variable, int) Hashtbl.t

  type storage = value list

  type closure =
    | Closure of Ast.declaration list * Ast.block list * Ast.expression option
    [@@deriving show]

  type stack_item =
      | Kontinuation of (closure * environment)
      | MethodReturnCheck of Ast.typ
  type kstack = stack_item list

  type _ Effect.t += SingleStep : (current * environment * storage * kstack) -> unit Effect.t

  val assign_new_values : environment -> storage -> Ast.variable list -> value list -> storage

  val step : current -> environment -> storage -> kstack -> unit

  val with_classes : Ast.classdef list -> (unit -> unit) -> unit
end

val run : Ast.t -> unit
