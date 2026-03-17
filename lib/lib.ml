open Effect
open Ppxlib
open Ast_helper
open Ast_builder.Default

let () =
  Driver.register_transformation
    ~impl:(Rewriter.walker#structure Context.empty)
    "conteff"
