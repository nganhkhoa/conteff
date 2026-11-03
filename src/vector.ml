(* A fixed-size mutable vector represented by its operations, see box.ml. *)

type 'a t = { length : int; get : int -> 'a; set : int -> 'a -> unit }

let create n default =
  let a = Array.make n default in
  { length = n; get = Array.get a; set = Array.set a }

let of_list l =
  let a = Array.of_list l in
  { length = Array.length a; get = Array.get a; set = Array.set a }

let make ~length ~get ~set = { length; get; set }

let length v = v.length

let get v i = v.get i

let set v i x = v.set i x

let to_list v = List.init v.length v.get
