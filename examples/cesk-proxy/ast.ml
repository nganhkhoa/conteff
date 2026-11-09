type name = string
  [@@deriving show, eq]
type fieldname = string
  [@@deriving show, eq]
type parameter = string
  [@@deriving show, eq]
type variable = string
  [@@deriving show, eq]
type classname = string
  [@@deriving show, eq]

type typ =
  | Real
  | Shape of shape
  | Empty
  [@@deriving show, eq]
and fieldtyp = fieldname * typ
  [@@deriving show, eq]
and methodtyp = name * typ list * typ
  [@@deriving show, eq]
and shape = fieldtyp list * methodtyp list
  [@@deriving show, eq]

type expression =
  | Num of float
  | Var of variable
  | Add of variable * variable
  | Div of variable * variable
  | Eq of variable * variable
  | NewClass of classname * variable list
  | FieldAccess of variable * variable
  | MethodCall of variable * variable * variable list
  | IsA of variable * classname
  [@@deriving show, eq]

type declaration =
  | Def of variable * expression
  [@@deriving show, eq]

type statement =
  | Assign of variable * expression
  | FieldAssign of variable * fieldname * expression
  | If0 of expression * block * block
  | While0 of expression * block
  [@@deriving show, eq]
and block =
  | Simple of statement
  | Complex of declaration list * statement list
  [@@deriving show, eq]

type classmethod =
  | Method of name * parameter list * declaration list * statement list * expression
  [@@deriving show, eq]

type classdef =
  | Class of classname * fieldname list * classmethod list * shape option
  [@@deriving show, eq]

type miximport =
  | Import of name
  | TypedImport of name * shape
  [@@deriving show, eq]

type mixmodule =
  | Module of name * miximport list * classdef
  | TypedModule of name * miximport list * classdef * shape
  [@@deriving show, eq]

type program = classdef list * declaration list * statement list * expression
  [@@deriving show, eq]

type mixsystem = mixmodule list * miximport list * declaration list * statement list * expression
  [@@deriving show, eq]

type t =
  | Program of program
  | MixedSystem of mixsystem
  [@@deriving show, eq]

