open EllipticCurve

exception Blame of string

module type ECDH_PROTOCOL = sig
  module EC : EllipticCurve.CURVE
  val encode_public_key : string -> string -> string -> point -> bytes
  val compute_shared_point : string -> string -> string -> Z.t -> bytes -> point
end

module MakeECDHProtocol (C : CURVE_PARAMS) = struct
  module EC = MakeEllipticCurve(C)

  let z_of_bytes b = Z.of_bits (Bytes.to_string b |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq)

  let bytes_of_z z len =
    let hex = Z.format (Printf.sprintf "%%0%dx" Stdlib.(len * 2)) z in
    let b = Bytes.create len in
    for i = 0 to Stdlib.(len - 1) do
      Bytes.set b i (char_of_int (int_of_string ("0x" ^ String.sub hex Stdlib.(i * 2) 2)))
    done;
    b

  let correctly_formatted encoded_bytes =
    let expected_len = Stdlib.(1 + (C.byte_len * 2)) in
    Bytes.length encoded_bytes = expected_len &&
    Bytes.get encoded_bytes 0 = '\x04'

  let is_valid_point pt =
    EC.is_in_field pt && EC.is_on_curve pt

  let not_infinity = function
    | Infinity -> false
    | _ -> true

  let valid_not_infinity p = is_valid_point p && not_infinity p

  let
    [@contract : correctly_formatted -> is_valid_point]
    decode_public_key (encoded_bytes : Bytes.t) : point =
      let x = z_of_bytes (Bytes.sub encoded_bytes 1 C.byte_len) in
      let y = z_of_bytes (Bytes.sub encoded_bytes Stdlib.(1 + C.byte_len) C.byte_len) in
      Affine (x, y)

  let
    [@contract : not_infinity -> correctly_formatted]
    encode_public_key (p : point) : Bytes.t =
      match p with
      | Infinity -> Bytes.create 0
      | Affine (x, y) ->
          let res = Bytes.create Stdlib.(1 + (C.byte_len * 2)) in
          Bytes.set res 0 '\x04';
          Bytes.blit (bytes_of_z x C.byte_len) 0 res 1 C.byte_len;
          Bytes.blit (bytes_of_z y C.byte_len) 0 res Stdlib.(1 + C.byte_len) C.byte_len;
          res

  let
    [@contract : any -> correctly_formatted -> valid_not_infinity]
    compute_shared_point (private_scalar : Z.t) (remote_pub_bytes : Bytes.t) : point =
    let remote_pub_point = decode_public_key remote_pub_bytes in
    EC.scalar_mult "scalar_mult" "compute_shared_secret" "scalar_mult" private_scalar remote_pub_point

end
