(* A mutable cell represented by its operations, so that a contract can
   interpose on reads and writes (a chaperone): a contracted box shares the
   underlying cell with the original, but checks values going through it. *)

type 'a t = { get : unit -> 'a; set : 'a -> unit }

let create v =
  let r = ref v in
  { get = (fun () -> !r); set = (fun x -> r := x) }

let make ~get ~set = { get; set }

let get b = b.get ()

let set b v = b.set v
