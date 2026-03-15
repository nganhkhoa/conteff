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
let x = 1
let x = 2
let y = 2
let y = 1
let use_x = x + 1
let use_y = (y "Duplicate" __FUNCTION__ "Duplicate.y") + 1
