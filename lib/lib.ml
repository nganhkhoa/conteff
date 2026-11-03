open Ppxlib

let () =
  Driver.register_transformation
    ~impl:(Rewriter.rewriter#structure Rewriter.empty)
    "conteff"
