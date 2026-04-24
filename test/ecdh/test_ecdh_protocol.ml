open Curves
open EcdhProtocol

module Protocol = MakeECDHProtocol(Secp256k1_Params)

let priv_a = Z.of_int 11111
let priv_b = Z.of_int 22222

let pub_a_point = Protocol.EC.scalar_mult "scalar_mult" "main1" "scalar_mult" priv_a Protocol.EC.generator
let pub_b_point = Protocol.EC.scalar_mult "scalar_mult" "main2" "scalar_mult" priv_b Protocol.EC.generator

let test_encode () =
  Printf.printf "--- Testing encode_public_key ---\n";

  Printf.printf "  [+] Encoding valid Affine point...\n";
  let payload = Protocol.encode_public_key "encode_public_key" "test_encode good point" "encode_public_key" pub_a_point in

  try
    let _ = Protocol.encode_public_key "encode_public_key" "test_encode Infinity" "encode_public_key" Infinity in
      Printf.printf "      -> Fail: violation not caught\n\n"
  with
  | EcdhProtocol.Blame msg ->
      Printf.printf "      -> Success: Caught violation -> %s\n\n" msg


let test_decode () =
  Printf.printf "--- Testing decode_public_key ---\n";
  let valid_payload = Protocol.encode_public_key "" "" "" pub_a_point in

  Printf.printf "  [+] Decoding perfectly formatted, valid curve point...\n";
  let decoded_point = Protocol.decode_public_key "" "" "" valid_payload in

  Printf.printf "  [+] Decoding truncated payload (Expect Failure)...\n";
  let bad_format = Bytes.sub valid_payload 0 32 in
  try
    let _ = Protocol.decode_public_key "" "test_decode bad input" "" bad_format in
    Printf.printf "      -> FATAL: Precondition [@contract: correctly_formatted] bypassed!\n"
  with
  | EcdhProtocol.Blame msg ->
      Printf.printf "      -> Success: %s breaks pre [@contract: correctly_formatted].\n\n" msg;

  Printf.printf "  [+] Decoding off-curve payload (Expect Failure)...\n";
  let malicious_payload = Bytes.copy valid_payload in
  (* Force X to be 1, which puts it off the secp256k1 curve *)
  Bytes.blit (Protocol.bytes_of_z Z.one 32) 0 malicious_payload 1 32;
  try
    let _ = Protocol.decode_public_key "decode_public_key" "test_decode output wrong point" "" malicious_payload in
    Printf.printf "      -> FATAL: Postcondition [@contract: is_valid_point] bypassed!\n"
  with
  | EcdhProtocol.Blame msg ->
      Printf.printf "      -> Success: %s breaks post [@contract: is_valid_point].\n\n" msg


let test_compute () =
  Printf.printf "--- Testing compute_shared_secret ---\n";

  let alice_payload = Protocol.encode_public_key "" "" "" pub_a_point in
  let bob_payload   = Protocol.encode_public_key "" "" "" pub_b_point in

  Printf.printf "  [+] Alice computing secret with Bob's payload...\n";
  let point_from_alice = Protocol.compute_shared_point "" "" "" priv_a bob_payload in

  Printf.printf "  [+] Bob computing secret with Alice's payload...\n";
  let point_from_bob = Protocol.compute_shared_point "" "" "" priv_b alice_payload in

  match point_from_alice, point_from_bob with
  | Affine (ax, ay), Affine (bx, by) ->
      if Z.equal ax bx && Z.equal ay by then begin
        let hex_x = Z.format "%x" ax in
        Printf.printf "      -> Success: Both parties derived the exact same point!\n";
        Printf.printf "      -> Shared X-Coordinate: %s...\n\n" (String.sub hex_x 0 16)
      end else begin
        Printf.printf "      -> FATAL: Points do not match! Math is broken.\n"
      end
  | _ ->
      Printf.printf "      -> FATAL: Unexpected Infinity result.\n"

let () =
  test_encode ();
  test_decode ();
  test_compute ();
