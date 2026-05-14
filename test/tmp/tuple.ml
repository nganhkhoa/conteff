open Conteff_lib

let t1 _ = true
let t1' _ = true
let t2 _ = true

let any _ = true

let [@contract : (((t1 -> t1') * t2) as 'tup) -> any]
  f (t : ((int -> int) * int)) : int =
  snd t

let main = fun () ->
  let pos = "f" in
  let neg = "main" in
  let cloc = "f" in
  let f = f pos neg cloc in

  let g = fun x -> x + 1 in

  let r = f (g, 123) in
  Printf.printf "result is %d\n" r

let () =
  Contract.run_with_effects main
