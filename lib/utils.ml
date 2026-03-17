open Ppxlib
let debug_vb_source (vb : value_binding) =
  let loc = vb.pvb_loc in

  (* 1. Wrap the binding in a dummy `let` structure item *)
  let dummy_str_item = Ast_builder.Default.pstr_value ~loc Nonrecursive [vb] in

  (* 2. Print the list of structure items (which is just our one item) *)
  let source_code = Format.asprintf "%a" Pprintast.structure [dummy_str_item] in

  Format.eprintf "--- VALUE BINDING SOURCE ---@.%s@.----------------------------@." source_code

let debug_expr_source (expr : expression) =
  let source_code = Format.asprintf "%a" Pprintast.expression expr in
  Format.eprintf "--- EXPRESSION SOURCE ---@.%s@.-------------------------@." source_code
