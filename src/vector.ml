type 'a t = ((int -> 'a) * (int -> 'a -> unit))

let get (type a) (vec : a t) (idx : int) =
  let (getfn, _) = vec in
  getfn idx

let set (type a) (vec : a t) (idx : int) (v : a) =
  let (_, setfn) = vec in
  setfn idx v

(* create a [mutable] vector with initial size
   returns a tuple of (handler, tuple of get/set function)
   basically, we encode vector as a tuple of get/set
   where we can impose contracts on these functions
 *)
let create (type a) (size : int) (default : a) =
  let module ThisVec = struct
    type _ Effect.t +=
      | Get : (int) -> a Effect.t
      | Set : (int * a) -> unit Effect.t

    let handler thunk =
      let open Effect.Deep in
      Effect.Deep.match_with thunk ()
      {
        retc = (fun x _ -> x);
        exnc = (fun e -> raise e);
        effc = (fun (type c) (eff : c Effect.t)
          : ((c, a array -> _) continuation -> a array -> _) option ->
          match eff with
          | Get idx -> Some (fun k (s : a array) ->
              let v = Array.get s idx in
              continue k v s)
          | Set (idx, v) -> Some (fun k (s : a array) ->
              let _ = Array.set s idx v in
              continue k () s)
          | _ -> None)
      }
      (* init every item with default
         internally use the base OCaml Array
       *)
      (Array.make size default)
  end
  in

  let vget idx = Effect.perform (ThisVec.Get idx) in
  let vset idx v  = Effect.perform (ThisVec.Set (idx, v)) in
  (ThisVec.handler, (vget, vset))

