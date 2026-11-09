(* Exercise both the exported boundary and a recursive transition: mutating the
   environment after the first check must be caught before the second step. *)
open Effect.Deep
open Machine
open Machine.Internal

let expect_step_blame f =
  match Conteff_lib.Contract.run f with
  | exception Conteff_lib.Contract.Blame { contract = "Machine.Internal.step"; _ } -> ()
  | exception e -> failwith ("unexpected exception: " ^ Printexc.to_string e)
  | () -> failwith "expected step contract violation"

let () =
  let env = Hashtbl.create 1 in
  Hashtbl.add env "x" 0;
  expect_step_blame (fun () -> step Searching env [] []);
  Hashtbl.clear env;
  let transitions = ref 0 in
  expect_step_blame (fun () ->
    match_with (fun () -> step (Expression (Ast.Num 1.)) env [] []) ()
      { retc = Fun.id;
        exnc = raise;
        effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | SingleStep _ -> Some (fun (k : (a, _) continuation) ->
              incr transitions;
              Hashtbl.add env "bad" 0;
              continue k ())
          | RuntimeReturn _ -> Some (fun _ -> failwith "invalid transition was accepted")
          | _ -> None) });
  assert (!transitions = 1)

(* Exercise method boundaries directly, including states the source typechecker
   normally rejects.  The proxy shape can also disagree with its class. *)
let run_method_call shape args =
  let fields = Hashtbl.create 0 in
  let obj = Object (0, "C", fields) in
  let receiver = match shape with None -> obj | Some s -> Proxy (obj, s) in
  let env = Hashtbl.create 2 in
  Hashtbl.add env "o" 0;
  Hashtbl.add env "x" 1;
  let method_def = Ast.Method ("m", ["p"], [], [], Ast.Var "p") in
  let classes = [Ast.Class ("C", [], [method_def], None)] in
  Conteff_lib.Contract.run (fun () ->
    with_classes classes (fun () ->
      match_with
        (fun () -> step (Expression (Ast.MethodCall ("o", "m", args)))
            env [Number 7.; receiver] []) ()
        { retc = Fun.id;
          exnc = raise;
          effc = (fun (type a) (eff : a Effect.t) ->
            match eff with
            | SingleStep _ -> Some (fun (k : (a, _) continuation) -> continue k ())
            | RuntimeReturn (Number n) -> Some (fun _ -> assert (n = 7.))
            | RuntimeReturn _ -> Some (fun _ -> failwith "unexpected return value")
            | _ -> None) }))

let expect_method_blame name f =
  match f () with
  | exception Conteff_lib.Contract.Blame { contract; _ } when contract = name -> ()
  | exception e -> failwith ("unexpected exception: " ^ Printexc.to_string e)
  | () -> failwith ("expected contract violation: " ^ name)

let () =
  let shape = Some ([], [("m", [Ast.Real], Ast.Real)]) in
  run_method_call None ["x"];
  run_method_call shape ["x"];
  (* Lookup selects the requested name and its first declaration. *)
  run_method_call (Some ([], [("other", [], Ast.Empty);
                              ("m", [Ast.Real], Ast.Real);
                              ("m", [], Ast.Empty)])) ["x"];
  List.iter (fun args ->
    expect_method_blame "Machine.Internal.assign_new_values"
      (fun () -> run_method_call None args);
    expect_method_blame "Machine.Internal.process_arguments"
      (fun () -> run_method_call shape args)) [[]; ["x"; "x"]];
  (* Shape and supplied arguments agree, but the implementation does not. *)
  expect_method_blame "Machine.Internal.assign_new_values"
    (fun () -> run_method_call (Some ([], [("m", [], Ast.Real)])) []);
  expect_method_blame "Machine.Internal.assign_new_values"
    (fun () -> run_method_call (Some ([], [("m", [Ast.Real; Ast.Real], Ast.Real)])) ["x"; "x"]);
  expect_method_blame "Machine.Internal.get_tmethod"
    (fun () -> run_method_call (Some ([], [])) ["x"])

(* The arity precondition runs before the fold can modify the environment. *)
let () =
  List.iter (fun (names, values) ->
    let env = Hashtbl.create 2 in
    Hashtbl.add env "existing" 42;
    expect_method_blame "Machine.Internal.assign_new_values" (fun () ->
      ignore (Conteff_lib.Contract.run (fun () ->
        assign_new_values env [] names values)));
    assert (Hashtbl.length env = 1 && Hashtbl.find env "existing" = 42))
    [(["existing"; "new"], [Number 1.]);
     (["existing"], [Number 1.; Number 2.])]

let () =
  let env = Hashtbl.create 2 in
  let one, two = Number 1., Number 2. in
  let storage = Conteff_lib.Contract.run (fun () ->
    assign_new_values env [Number 0.] ["a"; "b"] [one; two]) in
  assert (Hashtbl.find env "a" = 1 && Hashtbl.find env "b" = 2);
  match storage with
  | [last; first; Number 0.] -> assert (last == two && first == one)
  | _ -> failwith "unexpected storage after allocation"

(* A return check is part of the step contract, but successful shape
   conformance must still wrap an object before the continuation sees it. *)
let () =
  let env = Hashtbl.create 0 in
  let fields = Hashtbl.create 0 in
  let obj = Object (99, "C", fields) in
  let shape = ([], []) in
  expect_step_blame (fun () ->
    step (Value obj) env [] [MethodReturnCheck Ast.Real]);
  Conteff_lib.Contract.run (fun () ->
    match_with (fun () ->
      step (Value obj) env [] [MethodReturnCheck (Ast.Shape shape)]) ()
      { retc = (fun () -> failwith "expected return");
        exnc = raise;
        effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | SingleStep _ -> Some (fun (k : (a, _) continuation) -> continue k ())
          | RuntimeReturn (Proxy (original, actual_shape)) ->
              Some (fun _ -> assert (original == obj && actual_shape == shape))
          | RuntimeReturn _ -> Some (fun _ -> failwith "expected proxy")
          | _ -> None) })

(* Contract construction performs class lookup at the dependent-argument
   stage, and captures a method result before running the method lookup body. *)
let () =
  let method_a = Ast.Method ("m", [], [], [], Ast.Num 1.) in
  let method_b = Ast.Method ("m", [], [], [], Ast.Num 2.) in
  let class_a = Ast.Class ("C", [], [method_a], None) in
  let class_b = Ast.Class ("C", [], [method_b], None) in
  let run expression storage env class_for_call =
    let lookups = ref 0 in
    let execute () =
      Conteff_lib.Contract.run (fun () ->
        match_with (fun () -> step (Expression expression) env storage []) ()
          { retc = Fun.id;
            exnc = raise;
            effc = (fun (type a) (eff : a Effect.t) ->
              match eff with
              | GetClass "C" -> Some (fun (k : (a, _) continuation) ->
                  incr lookups;
                  continue k (Some (class_for_call !lookups)))
              | NewInstanceId -> Some (fun (k : (a, _) continuation) -> continue k 0)
              | SingleStep _ -> Some (fun (k : (a, _) continuation) -> continue k ())
              | RuntimeReturn _ -> Some (fun _ -> ())
              | _ -> None) })
    in
    (execute, lookups)
  in
  let execute, lookups = run (Ast.NewClass ("C", [])) [] (Hashtbl.create 0)
      (fun _ -> class_a) in
  execute ();
  assert (!lookups = 2);
  let env = Hashtbl.create 1 in
  Hashtbl.add env "o" 0;
  let obj = Object (0, "C", Hashtbl.create 0) in
  let method_call = Ast.MethodCall ("o", "m", []) in
  let execute, lookups = run method_call [obj] env (fun _ -> class_a) in
  execute ();
  assert (!lookups = 3);
  (* First lookup constructs the domain; the second snapshots the expected
     method; the third is the body. A changed class must fail the snapshot. *)
  let execute, lookups = run method_call [obj] env
      (fun n -> if n = 3 then class_b else class_a) in
  expect_method_blame "Machine.Internal.fetch_method" execute;
  assert (!lookups = 3)
