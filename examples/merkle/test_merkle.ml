let sha256_hasher s =
  Digestif.SHA256.(digest_string s |> to_raw_string |> Bytes.of_string)

let broken_hasher s =
  Bytes.of_string (s ^ "_too_short")

let test_contracts () =
  let calls = ref 0 in
  let hash s = incr calls; sha256_hasher s in
  let partial = MerkleTree.build hash in
  assert (!calls = 0);
  assert (MerkleTree.count_leaves (partial []) = 0);
  assert (!calls = 0);
  assert (MerkleTree.count_leaves (partial ["a"; "b"; "c"]) = 3);
  assert (!calls > 0);
  (match MerkleTree.build broken_hasher ["a"] with
   | _ -> failwith "expected invalid hash to be rejected"
   | exception Conteff_lib.Contract.Blame b ->
       assert (b.party = "external");
       assert (b.contract = "MerkleTree.build"));
  (* Check the dependent result independently of build's correct implementation. *)
  let incorrect =
    [%mon ((MerkleTree.any @-> MerkleTree.good_length) @->
      dep_args [(items, MerkleTree.any)]
        (fun items -> MerkleTree.equal_count items))
      ~pos:"provider" ~neg:"client" ~loc:"merkle"]
      (fun _hash _items -> MerkleTree.Empty)
  in
  (match incorrect sha256_hasher ["a"] with
   | _ -> failwith "expected incorrect leaf count to be rejected"
   | exception Conteff_lib.Contract.Blame b ->
       assert (b.party = "provider");
       assert (b.contract = "merkle"))

let%contract_main () =

  test_contracts ();
  let data = ["block_0"; "block_1"; "block_2"; "block_3"] in

  Printf.printf "Building tree with valid SHA256 hasher...\n%!";
  let tree = MerkleTree.build sha256_hasher data in
  Printf.printf "Success! Root created with %d leaves.\n\n%!" (MerkleTree.count_leaves tree);

  Printf.printf "Attempting build with broken hasher...\n%!";
  try
    let _ = MerkleTree.build broken_hasher data in
    Printf.printf "Error: Managed to bypass Higher-Order contract!\n%!"
  with
  | _ -> Printf.printf "🛡️  Contract Defense: Rejected invalid hasher before execution.\n\n%!";

  ()
