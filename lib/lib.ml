open Ppxlib

let () =
  Driver.register_transformation
    ~impl:(fun structure ->
      structure |> State_rewriter.rewrite |> Rewriter.rewriter#structure Rewriter.empty)
    "conteff"
