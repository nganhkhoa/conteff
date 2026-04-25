let sha256_hasher s =
  Digestif.SHA256.(digest_string s |> to_raw_string |> Bytes.of_string)

let broken_hasher s =
  Bytes.of_string (s ^ "_too_short")

let () =
  let data = ["block_0"; "block_1"; "block_2"; "block_3"] in

  Printf.printf "Building tree with valid SHA256 hasher...\n%!";
  let tree = MerkleTree.build "merkle.build" "main" "merkle.build" sha256_hasher data in
  Printf.printf "Success! Root created with %d leaves.\n\n%!" (MerkleTree.count_leaves tree);

  Printf.printf "Attempting build with broken hasher...\n%!";
  try
    let _ = MerkleTree.build "merkle.build" "main" "merkle.build" broken_hasher data in
    Printf.printf "Error: Managed to bypass Higher-Order contract!\n%!"
  with
  | _ -> Printf.printf "🛡️  Contract Defense: Rejected invalid hasher before execution.\n\n%!";

  ()
