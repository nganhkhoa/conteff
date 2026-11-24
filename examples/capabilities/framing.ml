(* Framing with explicit box operations. Run from the impl root:
   dune exec examples/capabilities/framing.exe *)
open Conteff_lib

(* Box IDs are indices into the list managed by [with_boxes]. The tuple is a
   single payload, as required by the effect-contract attribute. *)
type _ Effect.t +=
  | Get : int -> int Effect.t
  | Set : (int * int) -> unit Effect.t

let get id = Effect.perform (Get id)
let set id value = Effect.perform (Set (id, value))

let any_read _id = true
let any_result _result = true
let may_set allowed (id, _value) = List.mem id allowed

(* Reads are unrestricted; only writes to IDs in [allowed] cross this
   boundary. Each application installs a fresh boundary for the body. *)
let frame allowed =
  let permitted_write = may_set allowed in
  let[@effect_contract Get (any_read |> any_result)]
      [@effect_contract Set (permitted_write |> any_result)] guarded body =
    body ()
  in
  guarded

(* Trusted interpreter for box effects. Both reads and writes use the same
   indexed store; an invalid ID raises Invalid_argument. *)
let with_boxes (initial : int list) body =
  let boxes = ref initial in
  let valid id = id >= 0 && id < List.length !boxes in
  let check_id id = if not (valid id) then invalid_arg "invalid box id" in
  let result =
    Effect.Deep.match_with body ()
      { retc = Fun.id;
        exnc = raise;
        effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Get id -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
              check_id id;
              Effect.Deep.continue k (List.nth !boxes id))
          | Set (id, value) -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
              check_id id;
              boxes := List.mapi (fun i old -> if i = id then value else old) !boxes;
              Effect.Deep.continue k ())
          | _ -> None) }
  in
  result, !boxes

let expect_blame thunk =
  match thunk () with
  | _ -> failwith "expected a forbidden write to be blamed"
  | exception Contract.Blame { party; contract } ->
      assert (party = "Framing.frame.guarded");
      assert (contract = "Framing.frame.guarded")

let%contract_main () =
  let result, boxes = with_boxes [10; 20; 30] (fun () ->
    let guarded = frame [0; 2] in
    let observed = guarded (fun () ->
      (* The framed box may be read but not written. *)
      let framed = get 1 in
      set 0 (get 0 + 1);
      set 2 33;
      framed)
    in
    expect_blame (fun () -> guarded (fun () -> set 1 999; 0));
    (* The rejected request never reached the store handler. *)
    assert (get 1 = 20);
    (* Outside the boundary, writing the same box is allowed. *)
    set 1 21;
    observed)
  in
  assert (result = 20 && boxes = [11; 21; 33]);
  let (), unchanged = with_boxes [7; 8] (fun () ->
    expect_blame (fun () -> frame [] (fun () -> set 0 99));
    assert (frame [] (fun () -> get 0) = 7))
  in
  assert (unchanged = [7; 8]);
  print_endline "framing: all assertions passed"

(* Only explicit Get and Set effects crossing this boundary are covered.
   Native mutation and operations handled inside the body are not intercepted.
   The store handler and ID mapping must be trusted; IDs do not prove heap
   separation or prevent access through aliases or other effects. *)
