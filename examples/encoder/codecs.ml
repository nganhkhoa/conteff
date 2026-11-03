module type CODEC = sig
  val encode : string -> bytes
  val decode : bytes -> string
end

module XorCodec (K : sig val key : int end) : CODEC = struct
  let transform s =
    let b = Bytes.of_string s in
    Bytes.map (fun c -> char_of_int (int_of_char c lxor K.key)) b

  let encode s = transform s
  let decode b = Bytes.to_string (transform (Bytes.to_string b))
end

module Rot13Codec : CODEC = struct
  let shift c =
    let i = int_of_char c in
    if (i >= 65 && i <= 90) then char_of_int (65 + (i - 65 + 13) mod 26)
    else if (i >= 97 && i <= 122) then char_of_int (97 + (i - 97 + 13) mod 26)
    else c

  let encode s = Bytes.of_string (String.map shift s)
  let decode b = String.map shift (Bytes.to_string b)
end

module RleCodec : CODEC = struct
  let encode s =
    let res = Buffer.create (String.length s) in
    let i = ref 0 in
    while !i < String.length s do
      let char = s.[!i] in
      let count = ref 0 in
      while !i < String.length s && s.[!i] = char do
        incr count; incr i
      done;
      Buffer.add_string res (string_of_int !count);
      Buffer.add_char res char
    done;
    Buffer.to_bytes res

  let decode b =
    let s = Bytes.to_string b in
    let res = Buffer.create (String.length s * 2) in
    let i = ref 0 in
    try
      while !i < String.length s do
        let start = !i in
        while !i < String.length s && s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
        let count_str = String.sub s start (!i - start) in
        let count = int_of_string count_str in
        let char = s.[!i] in
        for _ = 1 to count do Buffer.add_char res char done;
        incr i
      done;
      Buffer.contents res
    with _ -> ""
end

module Base64Codec : CODEC = struct
  let encode s = Base64.encode_string s |> Bytes.of_string
  let decode b = Base64.decode_exn (Bytes.to_string b)
end

module WrongCodec : CODEC = struct
  (* a wrong codec
     encode: pad 0x00 bytes
     decode: remove 0x00 to the closest alignment

     bug: decode strips only if length not aligned
          but after encode, string is aligned
   *)
  let align = 4

  let encode s =
    let len = String.length s in
    let pad_len = if len mod align = 0 then 0 else align - (len mod align) in
    let b = Bytes.create (len + pad_len) in
    Bytes.blit_string s 0 b 0 len;
    for i = len to (len + pad_len - 1) do
      Bytes.set b i '\000'
    done;
    b

  let decode b =
    let len = Bytes.length b in
    if len > 0 && (len mod align <> 0) then
      let s = Bytes.to_string b in
      String.trim s
    else
      Bytes.to_string b
end
