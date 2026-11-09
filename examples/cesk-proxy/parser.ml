open Sexplib
open Sexplib.Sexp
open Effect
open Effect.Deep

type error =
  | InvalidNumber of string
  | InvalidName of string
  | InvalidExpr of Sexp.t
  | InvalidStmt of Sexp.t
  | InvalidDecl of Sexp.t
  | InvalidBlock of Sexp.t
  | MissingFinalExpr
  | EndOfStream
  | EmptyBlock

type _ Effect.t += ParseError : error -> 'a Effect.t

module Internal = struct

(* instead of passing sexpr around, we handle states with effects
   use Peek/Next to get the peek/next item in list,
   when moving into a block with a different scope, use Scope
   Scope will make Peek/Next get the item inside the current scope


   This is on-going work
 *)
type _ Effect.t +=
  | Next  : Sexp.t Effect.t
  | Peek  : Sexp.t option Effect.t
  | MustEmpty  : unit Effect.t
  | Scope : Sexp.t * (unit -> 'a) -> 'a Effect.t

type stream_error =
  | EndOfStream
  | TrailingStream

type _ Effect.t += StreamError : stream_error -> 'a Effect.t

let keywords = [
  "class";
  "new";
  "if0";
  "while0";
  "block";
]

let invalid_names = keywords @ [
  "=";
  "/";
  "+";
  "==";
  "-->"
]

let valid_name s =
  let len = String.length s in
  let is_alpha = String.for_all (fun c ->
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  ) s in

  let using_invalid_names = List.mem s invalid_names in
  let is_good_names = len > 0 && len <= 20 && is_alpha in

  if using_invalid_names || (not is_good_names)
  then false
  else true

let check_name s =
  if valid_name s
  then ()
  else Effect.perform (ParseError (InvalidName s))

let next () = perform Next
let peek () = perform Peek

let one p =
  match p () with
  | Some x -> x
  | None -> Effect.perform (ParseError EndOfStream)

let rec many p =
  match p () with
  | Some x -> x :: many p
  | None -> []

let rec many1 p =
  match p () with
  | Some x -> x :: many p
  | None -> Effect.perform (ParseError EndOfStream)

let fetch_name () =
  match next() with
  | Atom x -> x
  | _ -> Effect.perform (ParseError EndOfStream)


(* maps EndOfStream error to another effect
   that way, we can throw back custom error
 *)
let parse_with_error (p : unit -> 'a) e : 'a =
  try_with p ()
  { effc = fun (type b) (eff : b Effect.t) ->
    match eff with
    | StreamError EndOfStream -> Some(fun k ->
        perform e
    )
    | _ -> None
  }

let parse_expr () : Ast.expression =
  let parse_expr_name_or_number (s : Sexp.t) : Ast.expression =
    let s = match s with
      | Atom x -> x
      | _ -> Effect.perform (ParseError (InvalidExpr s))
    in
    match float_of_string_opt s with
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
  in

  let fetch_name_list () : string list =
    let get_one_name () =
      match peek() with
      | Some _ -> Some (fetch_name())
      | None -> None
    in
    many get_one_name
  in

  let parse_expr_special (fst : Sexp.t) : Ast.expression =
    match fst with
    | List _ -> Effect.perform (ParseError (InvalidExpr fst))
    | Atom "new" ->
        let classname = fetch_name() in
        let params = perform (Scope (next(), fetch_name_list)) in
        Ast.NewClass (classname, params)

    | Atom lhs ->
        match fetch_name() with
        | "+" ->
            let rhs = fetch_name() in
            Ast.Add (lhs, rhs)
        | "/" ->
            let rhs = fetch_name() in
            Ast.Div (lhs, rhs)
        | "==" ->
            let rhs = fetch_name() in
            Ast.Eq (lhs, rhs)
        | "isa" ->
            let classname = fetch_name() in
            check_name classname;
            Ast.IsA (lhs, classname)
        | "-->" ->
            let accessor = fetch_name() in
            begin
            match peek() with
            | None -> Ast.FieldAccess(lhs, accessor)
            | Some List _ ->
                let args = perform (Scope(next(), fetch_name_list)) in
                Ast.MethodCall (lhs, accessor, args)
            | _ -> Effect.perform (ParseError (InvalidExpr fst))
            end
        | _ -> Effect.perform (ParseError (InvalidExpr fst))
  in

  let fst = next() in
  let expr = match peek() with
  | Some Atom _ -> parse_expr_special (fst)
  | None -> parse_expr_name_or_number (fst)
  | Some x -> Effect.perform (ParseError (InvalidExpr x))
  in
  let _ = Effect.perform MustEmpty in
  expr

let rec parse_block () : Ast.block =
  let block = match peek() with
  | None -> Ast.Complex ([], [])
  | Some Atom "block" ->
      let _ = next() in (* skip block *)
      let decls = many try_parse_decl in
      let stmts = many1 try_parse_stmt in
      Ast.Complex (decls, stmts)
  | _ -> Ast.Simple (parse_stmt ())
  in
  let _ = Effect.perform MustEmpty in
  block

and parse_decl () : Ast.declaration =
  let _ = next() in (* def *)
  let name = fetch_name() in
  let expr = perform (Scope (next(), parse_expr)) in
  let _ = Effect.perform MustEmpty in
  Def (name, expr)

and try_parse_decl () : Ast.declaration option =
  (* force the correct form of def to not messing with people using def as variable names *)
  match peek () with
  | Some List [Atom "def"; Atom name; _] when (valid_name name) ->
      Some (perform (Scope (next(), parse_decl)))
  | _ -> None

and parse_stmt () : Ast.statement =
  (* we don't have a retry backtracking effect at the moment
     so we have to do it a little different
   *)

  let parse_if_stmt () : Ast.statement =
    let _ = next() in
    let condition = perform (Scope(next(), parse_expr)) in
    let truebranch = perform (Scope(next(), parse_block)) in
    let falsebranch = perform (Scope(next(), parse_block)) in
    Ast.If0 (condition, truebranch, falsebranch)
  in

  let parse_while_stmt () : Ast.statement =
    let _ = next() in
    let condition = perform (Scope(next(), parse_expr)) in
    let body = perform (Scope(next(), parse_block)) in
    Ast.While0 (condition, body)
  in

  let parse_either_assignment () : Ast.statement =
    (* An assignment RHS can be a parenthesized expression or an unparenthesized
       expression occupying the rest of the statement. *)
    let parse_rhs () =
      match peek () with
      | Some (List _) -> perform (Scope (next (), parse_expr))
      | _ -> parse_expr ()
    in
    let var = fetch_name() in
    let op = fetch_name() in
    match op with
    | "-->" ->
        let field = fetch_name() in
        let _ = next() in
        let expr = parse_rhs () in
        FieldAssign (var, field, expr)
    | "=" ->
        let expr = parse_rhs () in
        Assign (var, expr)
    | _ -> perform (ParseError EndOfStream)
  in

  let fst = peek() in
  let stmt = match fst with
  | Some Atom "if0" -> parse_if_stmt ()
  | Some Atom "while0" -> parse_while_stmt ()
  | Some _ -> parse_either_assignment ()
  (* unreachable *)
  | None -> perform (ParseError EndOfStream)
  in
  let _ = Effect.perform MustEmpty in
  stmt

and try_parse_stmt () : Ast.statement option =
  match peek () with
  | Some List (Atom _ :: Atom "=" :: _ :: _)
  | Some List (Atom ("if0" | "while0") :: _)
  | Some List (Atom _ :: Atom "-->" :: Atom _ :: Atom "=" :: _ :: _) ->
      let stmts = next() in
      let parsed = parse_with_error
        (fun () -> perform (Scope(stmts, parse_stmt)))
        (ParseError (InvalidStmt stmts))
      in
      Some parsed
  | _ -> None

let parse_fields () : Ast.fieldname list =
  let next_as_name_option () =
    match peek() with
    | Some(_) -> begin
      match next() with
        | Atom f -> Some f
        | _ -> None
      end
    | _ -> None
  in
  many next_as_name_option

let parse_method () : Ast.classmethod =
  let _ = next () in (* class keyword *)
  let name = fetch_name() in
  let params = perform (Scope (next(), parse_fields)) in
  let decls   = many try_parse_decl in
  let stmts   = many try_parse_stmt in
  let expr    = perform (Scope(next (), parse_expr)) in
  let _ = Effect.perform MustEmpty in
  Method (name, params, decls, stmts, expr)

let try_parse_method () : Ast.classmethod option =
  match peek() with
  | Some(m) -> Some (perform (Scope (next(), parse_method)))
  | _ -> None

let parse_class () : Ast.classdef =
  let _ = next () in (* class keyword *)
  let name = fetch_name() in
  let params = perform (Scope (next(), parse_fields)) in
  let methods = many try_parse_method in
  let _ = Effect.perform MustEmpty in
  Class (name, params, methods, None)

let try_parse_class () : Ast.classdef option =
  match peek () with
  | Some List (Atom "class" :: _) ->
    (* we know this a class, we want effect parse handler
       so make a scope with the whole (class ...)
       and parse from that
     *)
    Some (perform (Scope (next(), parse_class)))
  | _ -> None

let rec parse_real () : Ast.typ =
  Real

and try_parse_type () : Ast.typ option =
  match peek() with
  | Some Atom "REAL" ->
      Some (perform (Scope (next(), parse_real)))
  | Some List _ ->
      let s = perform (Scope (next(), parse_shape)) in
      Some (Shape s)
  | _ -> None

and parse_field_type () : Ast.fieldtyp option =
  match peek() with
  | Some _ ->
    let p = (fun () ->
      let name = fetch_name() in
      let t = one try_parse_type in
      let _ = Effect.perform MustEmpty in
      (name, t)
    ) in
    Some (perform (Scope(next(), p)))
  | None -> None

and parse_method_type () : Ast.methodtyp option =
  match peek() with
  | Some _ ->
    let p = (fun () ->
      let name = fetch_name() in
      let params_t = perform (Scope (next(), (fun () -> many try_parse_type))) in
      let ret_t = one try_parse_type in
      let _ = Effect.perform MustEmpty in
      (name, params_t, ret_t)
    ) in
    Some (perform (Scope (next(), p)))
  | None -> None

and parse_shape () : Ast.shape =
  let params = perform (Scope (next(), (fun () -> many parse_field_type))) in
  let methods = perform (Scope (next(), (fun () -> many parse_method_type))) in
  let _ = Effect.perform MustEmpty in
  (params, methods)

let parse_typed_import  () : Ast.miximport =
  let _ = next() in (* timport *)
  let name = fetch_name() in
  let shape = perform (Scope (next(), parse_shape)) in
  let _ = Effect.perform MustEmpty in
  TypedImport (name, shape)

let parse_import  () : Ast.miximport =
  let _ = next() in (* import *)
  let name = fetch_name() in
  let _ = Effect.perform MustEmpty in
  Import name

(* made three of these because
     module only need import
     typed module/mixed system need both import types
 *)
let try_parse_import () : Ast.miximport option =
  match peek () with
  | Some List [Atom "import"; Atom name] when (valid_name name) ->
      Some (perform (Scope (next(), parse_import)))
  | _ -> None

let try_parse_mixed_import () : Ast.miximport option =
  match peek () with
  | Some List [Atom "import"; Atom name] when (valid_name name) ->
      Some (perform (Scope (next(), parse_import)))
  | Some List [Atom "timport"; Atom name; _] when (valid_name name) ->
      Some (perform (Scope (next(), parse_typed_import)))
  | _ -> None

let parse_module () : Ast.mixmodule =
  let _ = next () in (* module keyword *)
  let name = fetch_name() in
  let imports = many try_parse_import in
  let cls = perform (Scope (next(), parse_class)) in
  let _ = Effect.perform MustEmpty in
  Module (name, imports, cls)

let parse_typed_module () : Ast.mixmodule =
  let _ = next () in (* module keyword *)
  let name = fetch_name() in
  let imports = many try_parse_mixed_import in
  let cls = perform (Scope (next(), parse_class)) in
  let shape = perform (Scope (next(), parse_shape)) in
  let _ = Effect.perform MustEmpty in
  TypedModule (name, imports, cls, shape)

let try_parse_mixed_module () : Ast.mixmodule option =
  match peek() with
  | Some List (Atom "module" :: _) ->
        Some (perform (Scope (next(), parse_module)))
  | Some List (Atom "tmodule" :: _) ->
        Some (perform (Scope (next(), parse_typed_module)))
  | _ -> None

let parse_mixed_system () : Ast.t =
  let modules = many try_parse_mixed_module in
  let imports = many try_parse_mixed_import in
  let decls   = many try_parse_decl in
  let stmts   = many try_parse_stmt in
  let main    =
    parse_with_error
    (fun () -> perform (Scope(next(), parse_expr)))
    (ParseError MissingFinalExpr)
  in
  let _ = Effect.perform MustEmpty in
  MixedSystem (modules, imports, decls, stmts, main)

(* stream_handler receives a parser returns anytype, a Sexp.t stream and returns the parsed
   for instance, stream_handler parse_program whole_sexp,
      parse_program returns program

    inside the parser, we can call Peek/Next to get the next item of the stream
    Peek will not advance the stream
    we call Scope(new_stream, next_parser) to run parser against with new_stream

    doing it this way, we don't care about the current stream
 *)
let rec stream_handler : type a. (unit -> a) -> Sexp.t list -> a =
  fun parser current_stream ->
  let run_parser = Effect.Deep.match_with parser ()
  {
    (* parser returns x, we bind it with a closure with stream *)
    retc = (fun x -> (fun stream -> x));
    exnc = raise;

    effc = fun (type c) (eff : c Effect.t) ->
      (match eff with
      | Next -> Some (fun k ->
          (fun (stream : Sexp.t list) ->
             match stream with
             | [] -> perform (StreamError EndOfStream)
             | h :: t -> (continue k h) t
          )
      )

      | Peek -> Some (fun k ->
          (fun (stream : Sexp.t list) ->
             match stream with
             | [] -> (continue k None) []
             | h :: t -> (continue k (Some h)) stream
          )
      )

      | Scope (inner, (next_parser : unit -> c)) -> Some (fun k ->
          (fun (outer_stream : Sexp.t list) ->
              let ss = match inner with
                | Atom _ -> [inner]
                | List s -> s
              in
              (* Printf.eprintf "Scope of %s\n" (Sexplib.Sexp.to_string (Sexplib.Sexp.List ss)); *)
              let result = stream_handler next_parser ss in
              (continue k result) outer_stream
          )
      )

      | MustEmpty -> Some (fun k ->
          (fun (stream : Sexp.t list) ->
             match stream with
             | [] -> (continue k ()) stream
             | _ :: _ -> perform (StreamError TrailingStream)
          )
      )

      | _ -> None

      : ((c, Sexp.t list -> a) continuation -> Sexp.t list -> a) option)
  } in
  run_parser current_stream
end

let parse_mixed_system (sexp : Sexp.t) : Ast.t =
  match sexp with
  | Atom _ -> failwith "Expected list"
  | List initial_stream ->
      Internal.stream_handler Internal.parse_mixed_system initial_stream

(*
  Parser runner walks the S-expression and convert to the AST
 *)
let run (parser : Sexp.t -> Ast.t) (sexp : Sexp.t) : Ast.t =
  let p = try_with parser (sexp)
  { effc = fun (type b) (eff : b Effect.t) ->
    match eff with
    | Internal.StreamError EndOfStream -> Some(fun k ->
        perform (ParseError EndOfStream)
    )
    | Internal.StreamError TrailingStream -> Some(fun k ->
        perform (ParseError EndOfStream)
    )
    | _ -> None
  }
  in
  p
