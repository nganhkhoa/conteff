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
    cookies = [("library-name", "test_manual")]
  }]
open Effect.Deep
exception Blame of string 
let f pos neg cloc (g : t1 -> t2) =
  let g : string -> string -> string -> t1 -> t2 =
    fun pos neg cloc (g_1 : t1) ->
      let g_1 : string -> string -> string -> t1 =
        fun pos neg cloc ->
          let module Contract =
            struct type _ Effect.t +=  
                     | V: t1 -> t1 Effect.t  end in
            let __handler__ =
              {
                Effect.Deep.retc = (fun x -> x);
                exnc = (fun e -> raise e);
                effc =
                  (fun (type a) (type b) (eff : a Effect.t) ->
                     match eff with
                     | Contract.V x ->
                         (Some
                            ((fun (k : (a, b) Effect.Deep.continuation) ->
                                if k1 x
                                then Effect.Deep.continue k x
                                else Effect.Deep.discontinue k (Blame pos))) : 
                         ((a, b) Effect.Deep.continuation -> b) option)
                     | _ -> None)
              } in
            Effect.Deep.match_with Effect.perform (Contract.V g_1)
              __handler__ in
      let g_1 : t1 = g_1 neg pos cloc in
      let __ret__ : string -> string -> string -> t2 =
        fun pos neg cloc ->
          let module Contract =
            struct type _ Effect.t +=  
                     | V: t2 -> t2 Effect.t  end in
            let __handler__ =
              {
                Effect.Deep.retc = (fun x -> x);
                exnc = (fun e -> raise e);
                effc =
                  (fun (type a) (type b) (eff : a Effect.t) ->
                     match eff with
                     | Contract.V x ->
                         (Some
                            ((fun (k : (a, b) Effect.Deep.continuation) ->
                                if k2 x
                                then Effect.Deep.continue k x
                                else Effect.Deep.discontinue k (Blame pos))) : 
                         ((a, b) Effect.Deep.continuation -> b) option)
                     | _ -> None)
              } in
            Effect.Deep.match_with Effect.perform (Contract.V (g g_1))
              __handler__ in
      __ret__ pos neg cloc in
  let g : t1 -> t2 = g neg pos cloc in
  let __ret__ : string -> string -> string -> t3 =
    fun pos neg cloc ->
      let module Contract =
        struct type _ Effect.t +=  
                 | V: t3 -> t3 Effect.t  end in
        let __handler__ =
          {
            Effect.Deep.retc = (fun x -> x);
            exnc = (fun e -> raise e);
            effc =
              (fun (type a) (type b) (eff : a Effect.t) ->
                 match eff with
                 | Contract.V x ->
                     (Some
                        ((fun (k : (a, b) Effect.Deep.continuation) ->
                            if k3 x
                            then Effect.Deep.continue k x
                            else Effect.Deep.discontinue k (Blame pos))) : 
                     ((a, b) Effect.Deep.continuation -> b) option)
                 | _ -> None)
          } in
        Effect.Deep.match_with Effect.perform (Contract.V (g 0)) __handler__ in
  __ret__ pos neg cloc
let () = let foo (x : t1) : t2= y in (f "B" __FUNCTION__ "B.f") foo
