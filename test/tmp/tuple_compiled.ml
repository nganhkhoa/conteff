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
let t1 _ = true
let t1' _ = true
let t2 _ = true
let any _ = true
let f pos neg cloc (t : ((int -> int) * int)) =
  let (t : string -> string -> string -> ((int -> int) * int)) =
    fun pos neg cloc ->
      let (t_0, t_1) = t in
      let (t_0 : string -> string -> string -> int -> int) =
        fun pos neg cloc (t_0_p1 : int) ->
          let (t_0_p1 : string -> string -> string -> int) =
            fun pos neg cloc ->
              Effect.perform (Contract.Flat (pos, t1, t_0_p1)) in
          let (t_0_p1 : int) = t_0_p1 neg pos cloc in
          let ret = t_0 t_0_p1 in
          let (ret : string -> string -> string -> int) =
            fun pos neg cloc ->
              Effect.perform (Contract.Flat (pos, t1', ret)) in
          let (ret : int) = ret pos neg cloc in ret in
      let (t_0 : int -> int) = t_0 pos neg cloc in
      let (t_1 : string -> string -> string -> int) =
        fun pos neg cloc -> Effect.perform (Contract.Flat (pos, t2, t_1)) in
      let (t_1 : int) = t_1 pos neg cloc in (t_0, t_1) in
  let (t : ((int -> int) * int)) = t neg pos cloc in
  let ret = snd t in
  let (ret : string -> string -> string -> int) =
    fun pos neg cloc -> Effect.perform (Contract.Flat (pos, any, ret)) in
  let (ret : int) = ret pos neg cloc in ret
let main () =
  let pos = "f" in
  let neg = "main" in
  let cloc = "f" in
  let f = f pos neg cloc in
  let g x = x + 1 in let r = f (g, 123) in Printf.printf "result is %d\n" r
let () = Contract.run_with_effects main
