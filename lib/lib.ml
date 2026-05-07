open Effect
open Ppxlib
open Ast_helper
open Ast_builder.Default

let () =
  (* Driver.register_transformation
    ~impl:(Effect_builder.walker#structure)
    "conteff_effect_generator";
 *)
  Driver.register_transformation
    ~impl:(Rewriter.walker#structure Context.empty)
    "conteff_contract";

