open Conteff_lib

let any _ = true
let bigger_10 x = x > 10
let smaller_20 x = x < 20

(* so vec is a tuple of functions
   placing contracts on function is possible
   placing contracts on tuple is also possible
 *)
let [@contract
  : ((smaller_20 -> bigger_10) * (smaller_20 -> any -> any) as 'tup)
  -> any]
  process (vec : int Vector.t) : unit =
  let v1 = Vector.get vec 10 in
  Printf.printf "vec[10]: before=%d\n" v1;
  let _ = Vector.set vec 10 9 in
  let v3 = Vector.get vec 10 in
  Printf.printf "vec[10]: after=%d\n" v3;

  let v1 = Vector.get vec 5 in
  let _ = Vector.set vec 5 (v1 + 1000) in
  let v3 = Vector.get vec 5 in
  Printf.printf "vec[5]: before=%d after=%d\n" v1 v3;
  ()

let main () =
  let pos = "process" in
  let neg = "main" in
  let cloc = "process" in
  let process = process pos neg cloc in

  let (vec_handler, vec) = Vector.create 20 11 in
  vec_handler (fun () -> process vec)

let () =
  Contract.run_with_effects main
