type monitor_type =
  | Scope of string
  | Argument
  | DependentArgument
  | ReturnValue
  | Nested of string

  | Variable
  [@@deriving show]

type monitor =
  { name : string;
    scope : string;
    typ  : monitor_type;
  }
  [@@deriving show]

type t =
  { monitors : monitor list; (* keep a scope of monitored variables *)
    current : string option; (* the current scope module/function *)
    scope   : string list; (* keep the current scope by appending module/let names *)
  }
  [@@deriving show]

let empty = { monitors = []; current = None; scope = []; }
