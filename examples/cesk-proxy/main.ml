open Effect.Deep

let main () =
  [%contract_main] (
    In_channel.input_all stdin
    |> Sexplib.Sexp.of_string
    |> Parser.run Parser.parse_mixed_system
    |> Validator.run
    |> Linker.run
    |> Machine.run)

let () =
  match_with main ()
  {
    retc = (fun _ -> ());

    exnc = (fun e ->
      match e with
      | Conteff_lib.Contract.Blame { contract; _ } -> Printf.printf "%S" ("protection captured: " ^ contract)
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
          let _ = match err with
          | Parser.InvalidNumber s -> "Invalid Number: " ^ s
          | Parser.InvalidName s -> "Invalid Name: " ^ s
          | Parser.InvalidExpr s -> "Invalid Expression: " ^ Sexplib.Sexp.to_string s
          | Parser.InvalidStmt s -> "Invalid Statement: " ^ Sexplib.Sexp.to_string s
          | Parser.InvalidDecl s -> "Invalid Declaration: " ^ Sexplib.Sexp.to_string s
          | Parser.InvalidBlock s -> "Invalid Block: " ^ Sexplib.Sexp.to_string s
          | Parser.MissingFinalExpr -> "Missing Final Expression"
          | Parser.EmptyBlock -> "Empty Block"
          | Parser.EndOfStream -> "End of Stream"
          ;
          in
          Printf.printf "\"parse error\""
        )
      | Validator.ValidatorError err -> Some (fun (_k: (c, _) continuation) ->
          match err with
          | Validator.UndeclareVariable var ->
              Printf.eprintf "Undeclare Variable: %s\n" var;
              Printf.printf "\"undeclared variable\""
          | Validator.UndeclareClass cls ->
              Printf.eprintf "Undeclare Class: %s\n" cls;
              Printf.printf "\"undeclared variable\""
          | Validator.UnknownModule m ->
              Printf.eprintf "Unknown module: %s\n" m;
              Printf.printf "\"unknown module\""
          | Validator.DuplicateModuleName ->
              Printf.printf "\"duplicate module name\""
          | Validator.DuplicateClassName ->
              Printf.printf "\"duplicate class name\""
          | Validator.DuplicateMethodName
          | Validator.DuplicateFieldName
          | Validator.DuplicateParameterName ->
              Printf.printf "\"duplicate method, field, or parameter name\""
          | Validator.ImportMixing ->
              Printf.printf "\"bad import\""
          | Validator.ClassShapeError ->
              Printf.printf "\"unexpected method, field, or parameter name\""
          | Validator.TypeError ->
              Printf.printf "\"type error\""
          | Validator.UnhandledStatement stmt ->
              Printf.eprintf "Validator Miss Case: %s\n" (Ast.show_statement stmt);
          | Validator.UnhandledExpression expr ->
              Printf.eprintf "Validator Miss Case: %s\n" (Ast.show_expression expr);
          | Validator.InternalError ->
              Printf.eprintf "Validator Internal Error\n";
        )
      | Machine.RuntimeError err -> Some (fun (_k: (c, _) continuation) ->
          let _ = match err with
          | Machine.Unimplemented -> Printf.eprintf "unimplemented"
          | Machine.SystemCannotRun -> Printf.eprintf "cannot run a system program"
          | Machine.FieldNotFound f -> Printf.eprintf "field %s not found" f
          | Machine.MethodNotFound m -> Printf.eprintf "method %s not found" m
          | Machine.ClassNotFound c -> Printf.eprintf "class %s not found" c
          | Machine.ClassInitWrongArity c -> Printf.eprintf "class %s wrong arity" c
          | Machine.MethodWrongArity (c,m) -> Printf.eprintf "%s->%s wrong arity" c m
          | Machine.ExpectedNumber -> Printf.eprintf "expected number"
          | Machine.ExpectedObject -> Printf.eprintf "expected object"
          | Machine.NotConform -> Printf.eprintf "conform error"
          | _ -> Printf.eprintf "other errors\n"
          in
          Printf.printf "\"run-time error\"";
        )
      | Machine.RuntimeReturn v -> Some (fun (_k: (c, _) continuation) ->
          match v with
          | Number n -> Printf.printf "%.1f" n
          | Object _ -> Printf.printf "\"object\""
          | Proxy _ -> Printf.printf "\"proxy\""
        )
      | Machine.Internal.SingleStep _ -> Some (fun k ->
          (* in the future, make this a debugger, let the effect carries machine state out *)
          continue k ()
        )
      | _ -> None
    )
  }
