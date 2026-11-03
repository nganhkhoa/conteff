(* ECDH client and server
 * ./ecdh.exe --port port runs as server
 * ./ecdh.exe --server ip:port runs as client connect to server
 * ./ecdh.exe --malform generates wrong points data
 *
 * ECDH idea:
 *  server generates s, and point A = g^s
 *  client generates r, and point B = g^r
 *  client sends server B
 *  server sends client A
 *  both agree on same point g^(s+r)
 *
 *  A, B is a pair of (X,Y) coordinates in some Curve (use secp256k1)
 *  Send a point is equal to send only X, encoded
 *
 * Contracts:
 *  To keep the logic neat, we use contract to filter "wrong" inputs
 *  - point received wrongly encoded
 *  - point received not in specified Curve
 *  - point generated must be in the curve
 *  - point agreed on must be in the curve as well
 *)

type point = Infinity | Affine of Z.t * Z.t
type curve = { p: Z.t; a: Z.t; b: Z.t; g: point; n: Z.t; byte_len: int }

exception Contract_Violation of string

module EllipticCurve = struct
  let modulo x p = let r = Z.rem x p in if Z.sign r < 0 then Z.add r p else r

  let add_points cv p1 p2 =
    match p1, p2 with
    | Infinity, p | p, Infinity -> p
    | Affine (x1, y1), Affine (x2, y2) ->
        if Z.equal x1 x2 && not (Z.equal y1 y2) then Infinity
        else
          let lam =
            if Z.equal x1 x2 then
              let num = Z.add (Z.mul (Z.of_int 3) (Z.mul x1 x1)) cv.a in
              let den_inv = Z.invert (Z.mul (Z.of_int 2) y1) cv.p in
              modulo (Z.mul num den_inv) cv.p
            else
              let num = Z.sub y2 y1 in
              let den_inv = Z.invert (Z.sub x2 x1) cv.p in
              modulo (Z.mul num den_inv) cv.p
          in
          let x3 = modulo (Z.sub (Z.sub (Z.mul lam lam) x1) x2) cv.p in
          let y3 = modulo (Z.sub (Z.mul lam (Z.sub x1 x3)) y1) cv.p in
          Affine (x3, y3)

  let scalar_mult cv k pt =
    let rec loop k_rem current_pt acc =
      if Z.equal k_rem Z.zero then acc
      else
        let next_acc = if Z.testbit k_rem 0 then add_points cv acc current_pt else acc in
        let next_pt = add_points cv current_pt current_pt in
        loop (Z.shift_right k_rem 1) next_pt next_acc
    in loop k pt Infinity
end

module Contract = struct
  let is_on_curve cv = function
    | Infinity -> true
    | Affine (x, y) ->
        let lhs = ObliviousMath.modulo (Z.mul y y) cv.p in
        let rhs = ObliviousMath.modulo (Z.add (Z.add (Z.mul x (Z.mul x x)) (Z.mul cv.a x)) cv.b) cv.p in
        Z.equal lhs rhs

  let is_in_field cv x y = Z.geq x Z.zero && Z.lt x cv.p && Z.geq y Z.zero && Z.lt y cv.p
end

module SecureECDH = struct
  let z_of_bytes b = Z.of_bits (Bytes.to_string b |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq)

  let bytes_of_z z len =
    let hex = Z.format (Printf.sprintf "%%0%dx" (len * 2)) z in
    let b = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set b i (char_of_int (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))
    done; b

  let decode_public_key cv encoded_bytes =
    let expected_len = 1 + (cv.byte_len * 2) in
    if Bytes.length encoded_bytes <> expected_len then raise (Contract_Violation "Format Error: Invalid length.");
    if Bytes.get encoded_bytes 0 <> '\x04' then raise (Contract_Violation "Format Error: Missing 0x04.");

    let x = z_of_bytes (Bytes.sub encoded_bytes 1 cv.byte_len) in
    let y = z_of_bytes (Bytes.sub encoded_bytes (1 + cv.byte_len) cv.byte_len) in

    if not (Contract.is_in_field cv x y) then raise (Contract_Violation "Algebra Error: Out of field bounds.");
    let pt = Affine (x, y) in
    if not (Contract.is_on_curve cv pt) then raise (Contract_Violation "Security Error: Point not on curve.");
    pt

  let encode_public_key cv = function
    | Infinity -> raise (Contract_Violation "Encoding Error: Cannot encode Infinity.")
    | Affine (x, y) ->
        let res = Bytes.create (1 + (cv.byte_len * 2)) in
        Bytes.set res 0 '\x04';
        Bytes.blit (bytes_of_z x cv.byte_len) 0 res 1 cv.byte_len;
        Bytes.blit (bytes_of_z y cv.byte_len) 0 res (1 + cv.byte_len) cv.byte_len;
        res

  let compute_shared_secret cv private_scalar remote_pub_bytes =
    let remote_pub_point = decode_public_key cv remote_pub_bytes in
    match ObliviousMath.scalar_mult cv private_scalar remote_pub_point with
    | Infinity -> raise (Contract_Violation "Security Error: Point at infinity.")
    | Affine (x, _) -> bytes_of_z x cv.byte_len
end

(* ========================================================================= *)
(* 2. APPLICATION LOGIC & NETWORKING                                         *)
(* ========================================================================= *)

(* SECP256k1 parameters for realistic testing *)
let p256k1 = {
  p = Z.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F";
  a = Z.zero;
  b = Z.of_int 7;
  g = Affine (
    Z.of_string "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798",
    Z.of_string "0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
  );
  n = Z.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141";
  byte_len = 32;
}

(* Helper to send/recv exactly N bytes *)
let send_exact fd bytes =
  let len = Bytes.length bytes in
  let _ = Unix.send fd bytes 0 len [] in ()

let recv_exact fd len =
  let buf = Bytes.create len in
  let rec loop pos =
    if pos = len then buf
    else
      let r = Unix.recv fd buf pos (len - pos) [] in
      if r = 0 then failwith "Connection closed by peer"
      else loop (pos + r)
  in loop 0

(* Executes the protocol for either client or server *)
let execute_protocol fd role malform =
  let priv_key = Z.of_int (if role = "Server" then 12345 else 67890) in (* Hardcoded for demo *)
  let pub_key = ObliviousMath.scalar_mult p256k1 priv_key p256k1.g in

  let payload_to_send =
    if malform then begin
      Printf.printf "[%s] MALFORM FLAG DETECTED. Generating point off-curve...\n%!" role;
      (* We send an Affine point where X and Y are in the field, but definitely not on the curve *)
      SecureECDH.encode_public_key p256k1 (Affine (Z.of_int 42, Z.of_int 42))
    end else
      SecureECDH.encode_public_key p256k1 pub_key
  in

  Printf.printf "[%s] Sending Public Key (%d bytes)...\n%!" role (Bytes.length payload_to_send);
  send_exact fd payload_to_send;

  Printf.printf "[%s] Waiting for peer Public Key...\n%!" role;
  let expected_len = 1 + (p256k1.byte_len * 2) in
  let peer_bytes = recv_exact fd expected_len in
  Printf.printf "[%s] Received %d bytes. Passing to Secure Contract...\n%!" role (Bytes.length peer_bytes);

  try
    let shared_secret = SecureECDH.compute_shared_secret p256k1 priv_key peer_bytes in
    let hex_secret = Z.format "%x" (SecureECDH.z_of_bytes shared_secret) in
    Printf.printf "[%s] SUCCESS: Shared Secret Computed -> %s\n%!" role hex_secret
  with
  | Contract_Violation msg ->
      Printf.printf "[%s] KILLED BY CONTRACT: %s\n%!" role msg;
      Printf.printf "[%s] Threat mitigated. Closing connection.\n%!" role

(* ========================================================================= *)
(* 3. CLI RUNNER                                                             *)
(* ========================================================================= *)

let run_server port malform =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen sock 1;
  Printf.printf "Server listening on port %d...\n%!" port;

  let (client_fd, _) = Unix.accept sock in
  Printf.printf "Client connected!\n%!";
  execute_protocol client_fd "Server" malform;
  Unix.close client_fd;
  Unix.close sock

let run_client ip port malform =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let inet_addr = Unix.inet_addr_of_string ip in
  Printf.printf "Connecting to %s:%d...\n%!" ip port;
  Unix.connect sock (Unix.ADDR_INET (inet_addr, port));
  execute_protocol sock "Client" malform;
  Unix.close sock

let () =
  let malform = ref false in
  let target = ref "" in
  let args = [
    ("--malform", Arg.Set malform, " Send an invalid point off-curve to test contract checking");
  ] in
  Arg.parse args (fun s -> target := s) "Usage: ./ecdh_app [--malform] [ip:port]";

  if !target = "" then
    run_server 8080 !malform
  else
    let parts = String.split_on_char ':' !target in
    let ip = List.hd parts in
    let port = int_of_string (List.nth parts 1) in
    run_client ip port !malform
