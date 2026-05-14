type 'a t = ((unit -> 'a) * ('a -> unit))

let get (type a) (box : a t) =
  let (getfn, _) = box in
  getfn ()

let set (type a) (box : a t) (v : a) =
  let (_, setfn) = box in
  setfn v

(* create a box with initial value `value`
   returns a tuple of (handler, tuple of get/set function)
   basically, we encode box as a tuple of get/set
   where we can impose contracts on these functions
 *)
let create (type a) (value : a) =
  let module ThisBox = struct
    type _ Effect.t +=
      | Get : a Effect.t
      | Set : a -> unit Effect.t

    let handler thunk =
      let open Effect.Deep in
      Effect.Deep.match_with thunk ()
      {
        retc = (fun x _ -> x);
        exnc = (fun e -> raise e);
        effc = (fun (type c) (eff : c Effect.t)
          : ((c, a -> _) continuation -> a -> _) option ->
          match eff with
          | Get -> Some (fun k (s : a) -> continue k s s)
          | Set x -> Some (fun k (_ : a) -> continue k () x)
          | _ -> None)
      }
      value
  end
  in

  let bget () = Effect.perform ThisBox.Get in
  let bset v  = Effect.perform (ThisBox.Set v) in
  (ThisBox.handler, (bget, bset))
