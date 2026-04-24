open Unix

open Curves
open EcdhProtocol
open Ecdh_runner

let port = ref 8080
let malicious = ref false
let curve_id = ref "secp256k1"

let () =
  let specs = [
    ("--port", Arg.Set_int port, " Port to listen on");
    ("--malicious", Arg.Set malicious, " Send malicious off-curve point");
    ("--curve", Arg.Set_string curve_id, " Curve to use: secp256k1 or p256r1");
  ] in
  Arg.parse specs (fun _ -> ()) "Usage: ./server.exe --port <port> [--malicious] [--curve <id>]";

  (* Setup Server Socket *)
  let fd = socket PF_INET SOCK_STREAM 0 in
  setsockopt fd SO_REUSEADDR true;
  bind fd (ADDR_INET (inet_addr_any, !port));
  listen fd 1;

  Printf.printf "[Server] Started on port %d using %s\n%!" !port !curve_id;
  let (client_fd, _) = accept fd in
  close fd; (* Stop listening for new connections to isolate the test *)

  let ic = in_channel_of_descr client_fd in
  let oc = out_channel_of_descr client_fd in

  (* Hardcoded Server Private Key for Demo *)
  let server_priv = Z.of_int 11111 in

  (* Dynamically select the protocol based on the curve_id *)
  if !curve_id = "secp256k1" then
    let module Protocol = MakeECDHProtocol(Secp256k1_Params) in
    let module App = MakeRunner(Protocol) in
    App.execute_exchange ic oc "Server" server_priv !malicious

  else if !curve_id = "p256r1" then
    let module Protocol = MakeECDHProtocol(P256_Params) in
    let module App = MakeRunner(Protocol) in
    App.execute_exchange ic oc "Server" server_priv !malicious

  else
    Printf.printf "[Server] Unknown curve: %s\n" !curve_id;

  close_in ic (* Closes the underlying file descriptor *)
