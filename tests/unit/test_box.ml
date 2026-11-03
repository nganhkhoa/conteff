(* Box and vector contracts: reads are checked like results (server
   blamed), writes like arguments (client blamed). *)

open Conteff_lib
open Helpers

let get_pass () = check_int 1 (fun () -> Box.get ([%mon box is_pos] (Box.create 1)))

let get_fail () =
  check_blame ~party:"k" ~contract:"j" (fun () ->
      Box.get ([%mon box is_pos ~pos:"k" ~neg:"l" ~loc:"j"] (Box.create 0)))

let set_fail () =
  check_party "l" (fun () -> Box.set ([%mon box is_pos ~pos:"k" ~neg:"l"] (Box.create 1)) (-1))

let set_then_get () =
  check_int 7 (fun () ->
      let b = [%mon box is_pos] (Box.create 1) in
      Box.set b 7;
      Box.get b)

let shared_cell () =
  (* the contracted box is a view on the same cell *)
  let raw = Box.create 1 in
  let b = [%mon box is_pos ~pos:"k"] raw in
  Box.set raw (-3);
  check_party "k" (fun () -> Box.get b)

let writes_visible () =
  let raw = Box.create 1 in
  let b = [%mon box is_pos] raw in
  Contract.run (fun () -> Box.set b 9);
  Alcotest.(check int) "raw" 9 (Box.get raw)

let box_of_functions () =
  let b = [%mon box (is_pos @-> yes) ~pos:"k" ~neg:"l"] (Box.create (fun x -> x)) in
  check_party "l" (fun () -> (Box.get b) (-1))

let box_hijacked () =
  check_int (-1) (fun () ->
      let b = [%mon box is_pos] (Box.create 1) in
      [%hijack] (Box.set b (-1));
      [%hijack] (Box.get b))

let let_bound_box () =
  check_party "Test_box.let_bound_box" (fun () ->
      let[@contract box is_even] b = Box.create 2 in
      Box.set b 3)

let vector_pass () =
  check_ints [ 1; 2; 3 ] (fun () -> Vector.to_list ([%mon vector is_pos] (Vector.of_list [ 1; 2; 3 ])))

let vector_get_fail () =
  check_party "k" (fun () -> Vector.get ([%mon vector is_pos ~pos:"k"] (Vector.of_list [ 1; 0 ])) 1)

let vector_set_fail () =
  check_party "l" (fun () ->
      Vector.set ([%mon vector is_pos ~pos:"k" ~neg:"l"] (Vector.create 3 1)) 2 (-4))

let vector_length () =
  Alcotest.(check int) "length" 4 (Vector.length ([%mon vector yes] (Vector.create 4 0)))

let () =
  Alcotest.run "box"
    [ ( "box",
        [ case "get pass" get_pass;
          case "get fail" get_fail;
          case "set fail" set_fail;
          case "set then get" set_then_get;
          case "shared cell" shared_cell;
          case "writes visible" writes_visible;
          case "box of functions" box_of_functions;
          case "hijacked" box_hijacked;
          case "let-bound box" let_bound_box ] );
      ( "vector",
        [ case "pass" vector_pass;
          case "get fail" vector_get_fail;
          case "set fail" vector_set_fail;
          case "length" vector_length ] ) ]
