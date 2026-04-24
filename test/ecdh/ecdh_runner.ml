open EllipticCurve

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02X" (int_of_char c))) b;
  Buffer.contents buf

let bytes_of_hex s =
  let len = String.length s / 2 in
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    let byte_str = "0x" ^ String.sub s (i * 2) 2 in
    Bytes.set b i (char_of_int (int_of_string byte_str))
  done;
  b

module MakeRunner (P : EcdhProtocol.ECDH_PROTOCOL) = struct
  let execute_exchange ic oc role_name private_key malicious =
    let pub_point = P.EC.scalar_mult "scalar_mult" "init_key" "runner" private_key P.EC.generator in

    (* MALICIOUS OVERRIDE: Set to off-curve point (1, 1) *)
    let point_to_send =
      if malicious then Affine (Z.one, Z.one) else pub_point
    in

    let payload = P.encode_public_key
      "encode_public_key" "execute_exchange->encode_public_key" "encode_public_key"
      point_to_send
    in
    let hex_payload = hex_of_bytes payload in

    Printf.fprintf oc "POINT: %s\n%!" hex_payload;
    Printf.printf "[%s] Sent -> POINT: %s...\n%!" role_name (String.sub hex_payload 0 16);

    Printf.printf "[%s] Waiting for peer...\n%!" role_name;
    try
      let line = input_line ic in

      if String.length line >= 7 && String.sub line 0 7 = "ERROR: " then begin
        Printf.printf "\n[%s] ❌ PEER ABORTED PROTOCOL: %s\n%!" role_name line;
        exit 1
      end

      else if String.length line >= 7 && String.sub line 0 7 = "POINT: " then begin
        let peer_hex = String.sub line 7 (String.length line - 7) in
        let peer_bytes = bytes_of_hex peer_hex in
        Printf.printf "[%s] Received POINT.\n%!" role_name;

        try
          let shared_pt = P.compute_shared_point
            "compute_shared_point" "execute_exchange->compute_shared_point" "compute_shared_point"
            private_key peer_bytes
          in
          match shared_pt with
          | Infinity -> Printf.printf "[%s] ❌ FATAL: Resulted in Point at Infinity\n%!" role_name
          | Affine (x, _y) ->
              let hex_x = Z.format "%x" x in
              Printf.printf "\n[%s] ✅ SUCCESS! Derived Shared X-Coordinate:\n" role_name;
              Printf.printf "    %s\n%!" hex_x

        with
        | exn ->
            let msg = Printexc.to_string exn in
            Printf.printf "\n[%s] 🛡️ CONTRACT DEFENSE TRIGGERED: %s\n" role_name msg;
            Printf.fprintf oc "ERROR: Contract Violation -> %s\n%!" msg;
            Printf.printf "[%s] Sent ERROR to peer and safely aborted.\n%!" role_name;
            exit 1
      end

      else begin
        Printf.printf "[%s] Unknown protocol message: %s\n%!" role_name line;
        Printf.fprintf oc "ERROR: Unrecognized command format.\n%!";
      end

    with End_of_file ->
      Printf.printf "[%s] Connection closed by peer.\n%!" role_name
end
