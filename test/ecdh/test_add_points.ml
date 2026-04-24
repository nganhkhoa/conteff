open Curves

let () =
  let k = Z.of_int 42 in
  let p1 = Secp256k1Curve.generator in
  let p2 = EllipticCurve.Affine (Z.one, Z.one) in

  (* p2 is not in Secp256k1Curve, blame main because we provide wrong p2 *)
  let result = Secp256k1Curve.add_points "add_points" "main" "add_points" p1 p2 in

  match result with
  | Infinity -> Printf.printf "Point at Infinity\n%!"
  | Affine (x, y) ->
      Printf.printf "Result X: %s\n" (Z.to_string x);
      Printf.printf "Result Y: %s\n%!" (Z.to_string y)
