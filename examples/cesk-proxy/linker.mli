type _ Effect.t +=
  | SynthesisResult : Ast.mixmodule list -> unit Effect.t

val run : Ast.t -> Ast.t
