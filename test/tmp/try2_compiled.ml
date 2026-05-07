[@@@ocaml.ppx.context
  {
    tool_name = "ppx_driver";
    include_dirs = [];
    hidden_include_dirs = [];
    load_path = ([], []);
    open_modules = [];
    for_package = None;
    debug = false;
    use_threads = false;
    use_vmthreads = false;
    recursive_types = false;
    principal = false;
    no_alias_deps = false;
    unboxed_types = false;
    unsafe_string = false;
    cookies = []
  }]
open Conteff_lib
let check_x v = Printf.printf "check_x %d\n" v; true
let check_g_p1_p1 v = Printf.printf "check_g_p1_p1 %d\n" v; true
let check_g_p1_ret v = Printf.printf "check_g_p1_ret %d\n" v; true
let check_g_ret p1 v = Printf.printf "check_g_ret %d->%d\n" (p1 10) v; true
let check_f_p1 x g v =
  Printf.printf "check_f_p1 %d->%d->%d\n" x
    (g (fun _ -> Printf.printf "call g'p1 in check_f_p1"; 20)) v;
  true
let check_f_ret x g p1 v =
  Printf.printf "check_f_ret %d->%d->%d->%d\n" x
    (g (fun _ -> Printf.printf "call g'p1 in check_f_ret"; 30)) p1 v;
  true
let f pos neg cloc (x : int) (g : (int -> int) -> int) =
  let (x : string -> string -> string -> int) =
    fun pos neg cloc -> Effect.perform (Contract.Flat (pos, check_x, x)) in
  let (x_dep : int) = Contract.run_with_effects (fun () -> x neg cloc cloc) in
  let (x : int) = x neg pos cloc in
  let (g : string -> string -> string -> (int -> int) -> int) =
    fun pos neg cloc (g_p1 : int -> int) ->
      let (g_p1 : string -> string -> string -> int -> int) =
        fun pos neg cloc (g_p1_p1 : int) ->
          let (g_p1_p1 : string -> string -> string -> int) =
            fun pos neg cloc ->
              Effect.perform (Contract.Flat (pos, check_g_p1_p1, g_p1_p1)) in
          let (g_p1_p1 : int) = g_p1_p1 neg pos cloc in
          let ret = g_p1 g_p1_p1 in
          let (ret : string -> string -> string -> int) =
            fun pos neg cloc ->
              Effect.perform (Contract.Flat (pos, check_g_p1_ret, ret)) in
          let (ret : int) = ret pos neg cloc in ret in
      let (g_p1_dep : int -> int) =
        fun p1 -> Contract.run_with_effects (fun () -> g_p1 neg cloc cloc p1) in
      let (g_p1 : int -> int) = g_p1 neg pos cloc in
      let ret = g g_p1 in
      let check_g_ret = check_g_ret g_p1_dep in
      let (ret : string -> string -> string -> int) =
        fun pos neg cloc ->
          Effect.perform (Contract.Flat (pos, check_g_ret, ret)) in
      let (ret : int) = ret pos neg cloc in ret in
  let (g_dep : (int -> int) -> int) =
    fun p1 -> Contract.run_with_effects (fun () -> g neg cloc cloc p1) in
  let (g : (int -> int) -> int) = g neg pos cloc in
  let ret y = g (fun x -> y * x) in
  let check_f_p1 = check_f_p1 x_dep g_dep in
  let check_f_ret = check_f_ret x_dep g_dep in
  let (ret : string -> string -> string -> int -> int) =
    fun pos neg cloc (ret_p1 : int) ->
      let (ret_p1 : string -> string -> string -> int) =
        fun pos neg cloc ->
          Effect.perform (Contract.Flat (pos, check_f_p1, ret_p1)) in
      let (ret_p1_dep : int) =
        Contract.run_with_effects (fun () -> ret_p1 neg cloc cloc) in
      let (ret_p1 : int) = ret_p1 neg pos cloc in
      let ret = ret ret_p1 in
      let check_f_ret = check_f_ret ret_p1_dep in
      let (ret : string -> string -> string -> int) =
        fun pos neg cloc ->
          Effect.perform (Contract.Flat (pos, check_f_ret, ret)) in
      let (ret : int) = ret pos neg cloc in ret in
  let (ret : int -> int) = ret pos neg cloc in ret
let f = f "f" "main" "f"
let main () = let g h = h 20 in let h = f 10 g in let _ = h 100 in ()
let () = Contract.run_with_effects main
