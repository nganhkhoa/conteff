open Effect.Deep

exception Blame of string

type _ Effect.t +=
  | Get : int Effect.t
  | Set : int -> unit Effect.t
  | Flat : (string * ('a -> bool) * 'a) -> 'a Effect.t

let checking_g'p1 v =
  Printf.printf "\tchecking g'p1 v=%d uwu=%d\n" v 1; true
let checking_g'p2'p1 v =
  Printf.printf "\tchecking g'p2'p1 v=%d uwu=%d\n" v 2; true
let checking_g'p2'r v =
  Printf.printf "\tchecking g'p2'r v=%d uwu=%d\n" v (Effect.perform Get);
  Effect.perform (Set ((Effect.perform Get) + 200));
  true
let checking_g'r p1 p2 v =
  Printf.printf "\tchecking g'r p1=%d v=%d uwu=%d\n" p1 v 4;
  let _ = p2 222 in
  true
let checking_x v = Printf.printf "\tchecking x v=%d uwu=%d\n" v 5; true
let checking_r g x v =
  Printf.printf "\tchecking r x=%d v=%d uwu=%d\n" x v 6;
  Printf.printf "\t\tdependent call to argument\n";
  Effect.perform (Set ((Effect.perform Get) + 100));
  let _ = g v (fun y -> y + 1000) in
  true

let f pos neg cloc (g : int -> (int -> int) -> int) (x : int) =
  let (g : string -> string -> string -> int -> (int -> int) -> int) =
    fun pos neg cloc (g_p1 : int) (g_p2 : int -> int) ->
      let (g_p1 : string -> string -> string -> int) =
        fun pos neg cloc ->
          Effect.perform (Flat (pos, checking_g'p1, g_p1)) in
      let (g_p1_dep : int) =
        g_p1 neg cloc cloc in
      let (g_p1 : int) = g_p1 neg pos cloc in
      let (g_p2 : string -> string -> string -> int -> int) =
        fun pos neg cloc (g_p2_p1 : int) ->
          let (g_p2_p1 : string -> string -> string -> int) =
            fun pos neg cloc ->
              Effect.perform (Flat (pos, checking_g'p2'p1, g_p2_p1)) in
          let (g_p2_p1 : int) = g_p2_p1 neg pos cloc in
          let ret = g_p2 g_p2_p1 in
          let (ret : string -> string -> string -> int) =
            fun pos neg cloc ->
              Effect.perform (Flat (pos, checking_g'p2'r, ret)) in
          let (ret : int) = ret pos neg cloc in ret in
      let (g_p2_dep : int -> int) =
        fun p1 -> g_p2 neg cloc cloc p1 in
      let (g_p2 : int -> int) = g_p2 neg pos cloc in
      let ret = g g_p1 g_p2 in
      let checking_g'r = checking_g'r g_p1_dep g_p2_dep in
      let (ret : string -> string -> string -> int) =
        fun pos neg cloc ->
          Effect.perform (Flat (pos, checking_g'r, ret)) in
      let (ret : int) = ret pos neg cloc in ret in
  let (g_dep : int -> (int -> int) -> int) =
    fun p1 p2 -> g neg cloc cloc p1 p2 in
  let (g : int -> (int -> int) -> int) = g neg pos cloc in
  let (x : string -> string -> string -> int) =
    fun pos neg cloc -> Effect.perform (Flat (pos, checking_x, x)) in
  let (x_dep : int) = x neg cloc cloc in
  let (x : int) = x neg pos cloc in
  let ret = let h x = x + 100 in g x h in
  let checking_r = checking_r g_dep x_dep in
  let (ret : string -> string -> string -> int) =
    fun pos neg cloc -> Effect.perform (Flat (pos, checking_r, ret)) in
  let (ret : int) = ret pos neg cloc in ret

let main () =
  let g x h = h (x + 10) in
  let pos = "f" in
  let neg = "main" in
  let cloc = "f" in
  let f = f pos neg cloc in let r = f g 1 in Printf.printf "result is %d\n" r

let rec with_state : type res. (unit -> res) -> int -> res * int =
  fun thunk init ->
  match_with thunk ()
  { retc = (fun x s -> (x, s));
    exnc = (fun e _ -> raise e);
    effc = fun (type a) (eff : a Effect.t) ->
      let handle_eff : ((a, int -> res * int) continuation -> int -> res * int) option =
        match eff with
        | Get -> Some (fun k s -> continue k s s)
        | Set x -> Some (fun k _ -> continue k () x)
        | _ -> None (* Flat bubbles seamlessly through this handler *)
      in handle_eff
  } init

let rec wrap_inner : type res. (unit -> res) -> res =
  fun thunk ->
  match_with thunk ()
  { retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = fun (type a) (eff : a Effect.t) ->
      match eff with
      | Flat (label, check, arg) ->
          Some (fun k ->
            (* Your insight: Just call wrap_inner again!
               If `check` calls Get/Set, they bubble straight up to `with_state` *)
            let ok = wrap_inner (fun () -> check arg) in

            if ok then continue k arg
            else discontinue k (Blame label))
      | _ -> None
  }

(* 3. The Public Enforcer: Wraps `main`, handles Flat, REJECTS Get/Set. *)
let rec wrap : type res. (unit -> res) -> int -> res * int =
  fun thunk init ->
  match_with thunk ()
  { retc = (fun x s -> (x, s));
    exnc = (fun e _ -> raise e);
    effc = fun (type a) (eff : a Effect.t) ->
      let handle_eff : ((a, int -> res * int) continuation -> int -> res * int) option =
        match eff with
        | Flat (label, check, arg) ->
            Some (fun k current_state ->
              let (ok, next_state) =
                with_state (fun () -> wrap_inner (fun () -> check arg)) current_state
              in

              if ok then continue k arg next_state
              else discontinue k (Blame label) next_state)

        | _ -> None
      in handle_eff
  } init

let () =
  let r, ctx = wrap main 0 in
  Printf.printf "context is %d\n" ctx;
  r


