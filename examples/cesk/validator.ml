open Ast

type error =
  | UndeclareVariable of string

type _ Effect.t += ValidatorError : error -> 'a Effect.t

let rec has_duplicate_names names =
  match names with
  | [] -> false
  | hd :: tail ->
      if List.mem hd tail then
        true
      else
        has_duplicate_names tail

(* a more generic inscope *)
let rec inscope (names : string list) (name : string) =
  match names with
  | [] -> Effect.perform (ValidatorError (UndeclareVariable name))
  | hd :: tail ->
      if hd = name then
        ()
      else
        inscope tail name

let get_def_name d =
  match d with
  | Def (name, _) -> name

let rec check_variable_stmt names stmt =
  match stmt with
  | Assign (var, expr) ->
      inscope names var;
      check_variable_expr names expr

  | If0 (e, t, f) ->
      check_variable_expr names e;
      check_variable_block names t;
      check_variable_block names f

  | While0 (e, b) ->
      check_variable_expr names e;
      check_variable_block names b

and check_variable_expr names expr =
  match expr with
  | Var v -> inscope names v
  | Add (lhs, rhs) ->
      inscope names lhs;
      inscope names rhs
  | Div (lhs, rhs) ->
      inscope names lhs;
      inscope names rhs
  | Eq (lhs, rhs) ->
      inscope names lhs;
      inscope names rhs
  | Num _ -> ()

and check_variable_block names block =
  match block with
  | Simple stmt -> check_variable_stmt names stmt
  | Complex (new_defs, stmts) ->
      let block_names = List.map get_def_name new_defs in
      check_variable_stmts (names @ block_names) stmts

and check_variable_stmts defs stmts =
  List.iter (check_variable_stmt defs) stmts;

(* go through the list of defs *)
and check_variable_decls names defs =
  match defs with
  | [] -> ()
  | Def (name, expr) :: rest ->
      check_variable_expr (name :: names) expr;
      check_variable_decls (name :: names) rest

(*
  Validator runner walks the tree with its own state
  detecting if the variable used is undeclared or not
 *)
let run (Program (defs, stmts, expr) as program) =
  let names = List.map get_def_name defs in
  check_variable_decls [] defs;
  check_variable_stmts names stmts;
  check_variable_expr names expr;
  program
