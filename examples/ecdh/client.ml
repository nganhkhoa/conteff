open Unix

open Curves
open EcdhProtocol
open Ecdh_runner

let server_addr = ref "127.0.0.1:8080"
let malicious = ref false
let curve_id = ref "secp256k1"

let () =
  let specs = [
    ("--server", Arg.Set_string server_addr, " Server IP:PORT");
    ("--malicious", Arg.Set malicious, " Send malicious off-curve point");
    ("--curve", Arg.Set_string curve_id, " Curve to use: secp256k1 or p256r1");
  ] in
  Arg.parse specs (fun _ -> ()) "Usage: ./client.exe --server <ip:port> [--malicious] [--curve <id>]";

  (* Parse IP:PORT *)
  let parts = String.split_on_char ':' !server_addr in
  let ip = List.hd parts in
  let port = int_of_string (List.nth parts 1) in

  (* Setup Client Socket *)
  let fd = socket PF_INET SOCK_STREAM 0 in
  Printf.printf "[Client] Connecting to %s:%d using %s...\n%!" ip port !curve_id;
  connect fd (ADDR_INET (inet_addr_of_string ip, port));

  let ic = in_channel_of_descr fd in
  let oc = out_channel_of_descr fd in

  (* Hardcoded Client Private Key for Demo *)
  let client_priv = Z.of_int 22222 in

  if !curve_id = "secp256k1" then
    let module Protocol = MakeECDHProtocol(Secp256k1_Params) in
    let module App = MakeRunner(Protocol) in
    App.execute_exchange ic oc "Client" client_priv !malicious

  else if !curve_id = "p256r1" then
    let module Protocol = MakeECDHProtocol(P256_Params) in
    let module App = MakeRunner(Protocol) in
    App.execute_exchange ic oc "Client" client_priv !malicious

  else
    Printf.printf "[Client] Unknown curve: %s\n" !curve_id;

  close_out oc
