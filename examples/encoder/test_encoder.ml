open Codecs
open Encoder

let run_codec_test name f =
  Printf.printf "--- Testing %s ---\n%!" name;
  try
    f ();
    Printf.printf "  [DONE] %s passed all checks.\n\n%!" name
  with
  | exn ->
      Printf.printf "  [BLAME] Contract Violation in %s: %s\n\n%!"
        name (Printexc.to_string exn)

module TestHonest = struct
  module Xor = MakeEncoder(XorCodec(struct let key = 0x42 end))
  module Rot13 = MakeEncoder(Rot13Codec)
  module Rle = MakeEncoder(RleCodec)
  module Base64 = MakeEncoder(Base64Codec)

  let run () =
    let input = "The quick brown fox jumps over 13 lazy dogs!" in

    run_codec_test "XorCodec (Honest)" (fun () ->
      let _ = Xor.encode input in ());

    run_codec_test "Rot13Codec (Honest)" (fun () ->
      let _ = Rot13.encode input in ());

    run_codec_test "RleCodec (Honest)" (fun () ->
      let rle_input = "aaaaabbbccccdde" in
      let _ = Rle.encode rle_input in ());

    run_codec_test "Base64Codec (Honest)" (fun () ->
      let _ = Base64.encode input in ())
end

module TestFail = struct
  module Wrong = MakeEncoder(WrongCodec)

  let run () =
    run_codec_test "WrongCodec (Clean Input)" (fun () ->
      let _ = Wrong.encode "Safe Strings" in ());

    run_codec_test "WrongCodec (Malicious Input)" (fun () ->
      let binary_input = "Non safe String" in
      let _ = Wrong.encode binary_input in ())
end

let test_dependent_contract () =
  let encoded = TestHonest.Xor.encode "round trip" in
  assert (TestHonest.Xor.decode encoded = "round trip");
  ignore (TestFail.Wrong.encode "abcd");
  match TestFail.Wrong.encode "abc" with
  | _ -> failwith "expected non-round-tripping codec to be rejected"
  | exception Conteff_lib.Contract.Blame b ->
      assert (b.party = "Encoder.MakeEncoder.encode");
      assert (b.contract = "Encoder.MakeEncoder.encode")

let%contract_main () =

  test_dependent_contract ();
  Printf.printf "=== STARTING CODEC CONTRACT VERIFICATION ===\n\n";
  TestHonest.run ();
  TestFail.run ();
  Printf.printf "=== ALL TESTS COMPLETED ===\n"
