open Sexplib
open Sexplib.Sexp
open Effect

type error =
  | InvalidNumber of string
  | InvalidName of string
  | InvalidExpr of Sexp.t
  | InvalidStmt of Sexp.t
  | InvalidDecl of Sexp.t
  | InvalidBlock of Sexp.t
  | MixedDeclStmt of Sexp.t
  | MissingFinalExpr
  | UnexpectedRest of Sexp.t list
  | EmptyBlock

type _ Effect.t += ParseError : error -> 'a Effect.t

let check_name s =
  let keywords = ["def"; "if0"; "while0"; "block"; "="; "/"; "+"; "=="] in
  if List.mem s keywords then
    Effect.perform (ParseError (InvalidName s));

  let len = String.length s in
  let is_alpha = String.for_all (fun c ->
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  ) s in

  if len > 0 && len <= 20 && is_alpha then ()
  else Effect.perform (ParseError (InvalidName s))

let rec parse_expr (sexp : Sexp.t) : Ast.expression =
  match sexp with
  | Atom s ->
    begin match float_of_string_opt s with
    | Some f ->
        if not (String.contains s '.') then
           Effect.perform (ParseError (InvalidNumber "Missing decimal point"));

        let parts = String.split_on_char '.' s in
        let decimals = match parts with
          | [_; d] -> d
          | _ ->
              Effect.perform (ParseError (InvalidNumber "Need at least one digit after decimal"))
        in
        if String.length decimals > 1 then
           Effect.perform (ParseError (InvalidNumber "Too many decimal digits"));

        if f < -1000.0 || f > 1000.0 then
           Effect.perform (ParseError (InvalidNumber "Out of range"));

        Ast.Num f

    | None ->
        check_name s;
        Ast.Var s
    end
  | List [Atom lhs; Atom "+"; Atom rhs] ->
      check_name lhs;
      check_name rhs;
      Ast.Add (lhs, rhs)
  | List [Atom lhs; Atom "/"; Atom rhs] ->
      check_name lhs;
      check_name rhs;
      Ast.Div (lhs, rhs)
  | List [Atom lhs; Atom "=="; Atom rhs] ->
      check_name lhs;
      check_name rhs;
      Ast.Eq (lhs, rhs)
  | _ -> Effect.perform (ParseError (InvalidExpr sexp))

and parse_stmt (sexp : Sexp.t) : Ast.statement =
  match sexp with
  | List [Atom "if0"; e; t; f] -> Ast.If0 (parse_expr e, parse_block t, parse_block f)
  | List [Atom "while0"; e; b] -> Ast.While0 (parse_expr e, parse_block b)
  | List [Atom var; Atom "="; e] ->
      check_name var;
      Ast.Assign (var, parse_expr e)
  | _ -> Effect.perform (ParseError (InvalidStmt sexp))

and parse_block (sexp : Sexp.t) : Ast.block =
  match sexp with
  | List (Atom "block" :: rest) ->
      let (defs, stmts) = parse_defs_stmts rest [] [] in
      if List.is_empty stmts then
        Effect.perform (ParseError (InvalidStmt sexp))
      else
        Ast.Complex (defs, stmts)
  | stmt -> Ast.Simple (parse_stmt stmt)

and parse_def (sexp : Sexp.t) : Ast.declaration =
  match sexp with
  | List [Atom "def"; Atom var; expr] ->
      check_name var;
      Ast.Def (var, parse_expr expr)
  | _ -> perform (ParseError (InvalidDecl sexp))

and parse_defs_stmts
    (sexps : Sexp.t list)
    (defs : Ast.declaration list)
    (stmts : Ast.statement list)
    : (Ast.declaration list * Ast.statement list) =
  match sexps with
  | [] -> (List.rev defs, List.rev stmts)

  | (List (Atom "def" :: _) as def) :: rest ->
      if stmts = [] then
        parse_defs_stmts rest (parse_def def :: defs) stmts
      else
        perform (ParseError (MixedDeclStmt def))

  | stmt :: rest ->
      parse_defs_stmts rest defs (parse_stmt stmt :: stmts)

let parse (sexp : Sexp.t) : Ast.t =
  match sexp with
  | Atom _ -> perform (ParseError MissingFinalExpr)
  | List items ->
      match List.rev items with
      | [] -> perform (ParseError MissingFinalExpr)
      | [e] -> Ast.Program ([], [], parse_expr e)
      | last :: rest ->
          let (defs, stmts) = parse_defs_stmts (List.rev rest) [] [] in
          Ast.Program (defs, stmts, parse_expr last)

(*
  Parser runner walks the S-expression and convert to the AST
 *)
let run (sexp : Sexp.t) : Ast.t = parse sexp
