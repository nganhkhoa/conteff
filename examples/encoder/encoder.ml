open Codecs

exception Blame of string

let any _ = true

module MakeEncoder (C : CODEC) = struct
  let decode_able input encoded =
    String.equal input (C.decode encoded)

  let
    [@contract dep_args
      [
        (input, any)
      ]
      (fun input -> decode_able input)
    ]
    encode (s : string) : bytes =
      C.encode s

  let decode (b : bytes) : string =
    C.decode b
end
