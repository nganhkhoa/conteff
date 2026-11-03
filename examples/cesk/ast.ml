type variable = string
  [@@deriving show]

type expression =
  | Num of float
  | Var of variable
  | Add of variable * variable
  | Div of variable * variable
  | Eq of variable * variable
  [@@deriving show]

type declaration =
  | Def of variable * expression
  [@@deriving show]

type statement =
  | Assign of variable * expression
  | If0 of expression * block * block
  | While0 of expression * block
  [@@deriving show]
and block =
  | Simple of statement
  | Complex of declaration list * statement list
  [@@deriving show]

type program =
  | Program of declaration list * statement list * expression
  [@@deriving show]

type t = program
