open Ast

type error =
  | DivisionZero
  | Unimplemented

type _ Effect.t += RuntimeError : error -> 'a Effect.t

type value =
  | Number of float
  [@@deriving show]

type _ Effect.t += RuntimeReturn : value -> 'a Effect.t

type current =
  | Expression of expression
  | Value of value
  | Searching
  [@@deriving show]

(* mapping string -> storage index *)
type environment = (variable, int) Hashtbl.t

(* mapping idx -> value

  Ocaml list performs best if prepend (add to head)
  index 0 -> list size - 1
  index 1 -> list size - 1 - 1
  index n -> list size -1 - n
 *)
type storage = value list


(* statement or block

   because block are also defined inside While0 and If0
   we can uniform them into block
 *)
type closure =
  | Closure of declaration list * block list * expression option
  [@@deriving show]

(* kontinuation stack

  popping the stack by List.tail
 *)
type kontinuation = (closure * environment) list

let rec step (c : current) (e : environment) (s : storage) (k : kontinuation) =
  let print_state (e : environment) (s : storage) =
    Hashtbl.iter (fun var loc ->
      let item = List.nth s (List.length s - 1 - loc) in
      Printf.eprintf "%s [%d] -> %s\n" var loc (show_value item)
    ) e;
    Printf.eprintf "==\n";
  in

  (* match k with
  | (closure, _) :: _ ->
      prerr_endline (show_closure closure);
      prerr_endline (show_current c);
  | [] ->
      prerr_endline "stack empty";
      prerr_endline (show_current c);
  ;
  print_state e s; *)

  (* cannot match e because  is a hash table, we can case on e *)
  match (c, s, k) with
  (* done, nothing else to evaluate *)
  | Value v, _, [] -> Effect.perform (RuntimeReturn v)

  (* expressions *)
  | Expression Num n, _, _ -> step (Value (Number n)) e s k
  | Expression Var var, _, _ ->
      let idx = Hashtbl.find e var in
      let v = List.nth s (List.length s - 1 - idx) in
      step (Value v) e s k
  | Expression Add (lhs, rhs), _, _ ->
      let idx1 = Hashtbl.find e lhs in
      let v1 =
        match List.nth s (List.length s - 1 - idx1) with
        | Number number1 -> number1
      in
      let idx2 = Hashtbl.find e rhs in
      let v2 =
        match List.nth s (List.length s - 1 - idx2) with
      | Number number2 -> number2
      in
      step (Value (Number (v1 +. v2))) e s k
  | Expression Div (lhs, rhs), _, _ ->
      let idx1 = Hashtbl.find e lhs in
      let v1 =
        match List.nth s (List.length s - 1 - idx1) with
        | Number number1 -> number1
      in
      let idx2 = Hashtbl.find e rhs in
      let v2 =
        match List.nth s (List.length s - 1 - idx2) with
        | Number 0.0 ->  Effect.perform (RuntimeError (DivisionZero));
        | Number number2 -> number2
      in
      step (Value (Number (v1 /. v2))) e s k
  | Expression Eq (lhs, rhs), _, _ ->
      let idx1 = Hashtbl.find e lhs in
      let v1 =
        match List.nth s (List.length s - 1 - idx1) with
        | Number number1 -> number1
      in
      let idx2 = Hashtbl.find e rhs in
      let v2 =
        match List.nth s (List.length s - 1 - idx2) with
        | Number number2 -> number2
      in
      let result = match (v1 = v2) with
        | true -> 0.0
        | false -> 1.0
      in
      step (Value (Number result)) e s k

  (* blocks and definition *)

  | Searching, _, ((Closure ((Def (_, expr)) :: _, _, _), _) :: _) ->
      step (Expression expr) e s k

  | Value v, _, ((Closure ((Def (var, _)) :: defs, stmts, expr), env) :: krest) ->
      let closure = Closure (defs, stmts, expr) in
      let idx = List.length s in
      Hashtbl.replace e var idx;
      step Searching e (v :: s) ((closure, env) :: krest)

  (* note: search encounters nested block *)
  | Searching, _, ((Closure ([], Complex (defs, stmts) :: rstmt, expr), env) :: krest) ->
      let nested_stmts = List.map (fun s -> Simple s) stmts in
      let cl = Closure (defs, nested_stmts, None) in
      let k = Closure ([], rstmt, expr) in
      (* make a new scope by cloning the current mappings *)
      step Searching (Hashtbl.copy e) s ((cl, e) :: (k, env) :: krest)

  | Searching, _, ((Closure ([], [], None), old_e) :: krest) -> step Searching old_e s krest
  | Searching, _, ((Closure ([], [], (Some r)), _) :: krest) -> step (Expression r) e s k
  | Value _, _, ((Closure ([], [], (Some _)), old_e) :: krest) -> step c old_e s krest

  (* assignment *)

  | Searching, _, ((Closure ([], Simple (Assign (_, rhs)) :: rstmt, _), _) :: krest) ->
      step (Expression rhs) e s k
  | Value v, s, ((Closure ([], Simple (Assign (var, _)) :: rstmt, expr), env) :: krest) ->
      let idx = Hashtbl.find e var in
      let new_s =
        List.mapi (fun i old_v -> if i = (List.length s - 1 - idx) then v else old_v) s
      in
      let closure = Closure ([], rstmt, expr) in
      step Searching e new_s ((closure, env) :: krest)

  (* while loops *)

  | Searching, _, ((Closure ([], Simple (While0 (tst, body)) :: _, _), _) :: krest) ->
      step (Expression tst) e s k
  | Value (Number 0.), _, ((Closure ([], (Simple (While0 (tst, body)) as while_stmt) :: rstmt, expr), env) :: krest) ->
      let closure = Closure ([], (body :: while_stmt :: rstmt), expr) in
      step Searching e s ((closure, env) :: krest)
  | Value _, _, ((Closure ([], Simple (While0 (tst, body)) :: rstmt, expr), env) :: krest) ->
      let closure = Closure ([], rstmt, expr) in
      step Searching e s ((closure, env) :: krest)

  (* conditionals *)

  | Searching, s, ((Closure ([], Simple (If0 (tst, thn, els)) :: rstmt, expr), env) :: krest) ->
      step (Expression tst) e s k
  | Value (Number 0.), s, ((Closure ([], Simple (If0 (tst, thn, els)) :: rstmt, expr), env) :: krest) ->
      let closure = Closure ([], thn :: rstmt, expr) in
      step Searching e s ((closure, env) :: krest)
  | Value _, s, ((Closure ([], Simple (If0 (tst, thn, els)) :: rstmt, expr), env) :: krest) ->
      let closure = Closure ([], els :: rstmt, expr) in
      step Searching e s ((closure, env) :: krest)

  (* debug *)
  | _ ->
      match k with
      | (closure, _) :: _ ->
          prerr_endline (show_closure closure);
          prerr_endline (show_current c);
      | [] ->
          prerr_endline "stack empty";
          prerr_endline (show_current c);
      ;
      Effect.perform (RuntimeError Unimplemented)

(* Machine runner runs the program according to CESK machine *)
let run program =
  prerr_endline (show_program program);
  let env = Hashtbl.create 0 in
  let (Program (defs, stmts, expr)) = program in
  let closure = Closure (defs, List.map (fun s -> Simple s) stmts, Some expr) in
  let k = [(closure, env)] in
  step Searching env [] k

