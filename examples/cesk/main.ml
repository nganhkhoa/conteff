open Effect.Deep

let string_of_parser_error err =
  match err with
  | Parser.InvalidNumber s -> "Invalid Number: " ^ s
  | Parser.InvalidName s -> "Invalid Name: " ^ s
  | Parser.InvalidExpr s -> "Invalid Expression: " ^ Sexplib.Sexp.to_string s
  | Parser.InvalidStmt s -> "Invalid Statement: " ^ Sexplib.Sexp.to_string s
  | Parser.InvalidDecl s -> "Invalid Declaration: " ^ Sexplib.Sexp.to_string s
  | Parser.InvalidBlock s -> "Invalid Block: " ^ Sexplib.Sexp.to_string s
  | Parser.MixedDeclStmt s -> "Mixed Definitions and Statements: " ^ Sexplib.Sexp.to_string s
  | Parser.MissingFinalExpr -> "Missing Final Expression"
  | Parser.UnexpectedRest _ -> "Unexpected elements after valid code"
  | Parser.EmptyBlock -> "Empty Block"

let () =
  let pipeline () =
    In_channel.input_all stdin
    |> Sexplib.Sexp.of_string
    |> Parser.run
    |> Validator.run
    |> Machine.run
  in

  match_with pipeline ()
  {
    retc = (fun _ -> ());

    exnc = (fun e ->
      match e with
      | Sexplib.Sexp.Parse_error err ->
          Printf.eprintf "Sexp Syntax Error: %s\n" err.err_msg
      | Failure msg ->
          Printf.eprintf "System Failure: %s\n" msg
      | _ ->
          Printf.eprintf "Unknown Exception: %s\n" (Printexc.to_string e)
    );

    effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Parser.ParseError err -> Some (fun (_k: (c, _) continuation) ->
          Printf.eprintf "\nParser Logic Error: %s\n" (string_of_parser_error err);
          Printf.printf "\"parse error\""
        )
      | Validator.ValidatorError err -> Some (fun (_k: (c, _) continuation) ->
          match err with
          | Validator.UndeclareVariable var ->
              Printf.eprintf "Undeclare Error: %s\n" var;
              Printf.printf "\"undeclared variable\""
        )
      | Machine.RuntimeError err -> Some (fun (_k: (c, _) continuation) ->
          match err with
          | Machine.Unimplemented -> print_endline "unimplemented"
          | _ -> Printf.printf "\"runtime error\""
        )
      | Machine.RuntimeReturn v -> Some (fun (_k: (c, _) continuation) ->
          match v with
          | Number n -> Printf.printf "%.1f" n
        )
      | _ -> None
    )
  }
