type monitor_type =
  | Scope of string
  | Argument
  | DependentArgument
  | ReturnValue
  | Nested of string
  [@@deriving show]

type monitor =
  { name : string;
    typ  : monitor_type;
  }
  [@@deriving show]

type t =
  { monitors : monitor list; (* keep a scope of monitored variables *)
    current : string option; (* the current scope module/function *)
  }
  [@@deriving show]

let empty = { monitors = []; current = None; }
