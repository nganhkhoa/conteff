exception Blame of string

let any _ = true

type point = Infinity | Affine of Z.t * Z.t

module type CURVE_PARAMS = sig
  val p : Z.t
  val a : Z.t
  val b : Z.t
  val g : point
  val n : Z.t
  val byte_len : int
end

module type CURVE = sig
  val modulo : Z.t -> Z.t -> Z.t

  val is_in_field : point -> bool
  val is_on_curve : point -> bool

  val add_points : string -> string -> string -> point -> point -> point
  val scalar_mult : string -> string -> string -> Z.t -> point -> point

  val generator : point
  val order : Z.t
end

module MakeEllipticCurve (C : CURVE_PARAMS) = struct
  let modulo x p =
    let r = Z.rem x p in
    if Z.sign r < 0 then Z.add r p else r

  let is_in_field = function
    | Infinity -> true
    | Affine (x, y) ->
        Z.geq x Z.zero && Z.lt x C.p && Z.geq y Z.zero && Z.lt y C.p

  let is_on_curve = function
    | Infinity -> true
    | Affine (x, y) ->
        let lhs = modulo (Z.mul y y) C.p in
        let rhs_x3 = Z.mul x (Z.mul x x) in
        let rhs_ax = Z.mul C.a x in
        let rhs = modulo (Z.add (Z.add rhs_x3 rhs_ax) C.b) C.p in
        Z.equal lhs rhs

  let
    [@contract : is_on_curve -> is_on_curve -> is_on_curve]
    add_points (p1 : point) (p2 : point) : point =
    match p1, p2 with
    | Infinity, p | p, Infinity -> p
    | Affine (x1, y1), Affine (x2, y2) ->
        if Z.equal x1 x2 && not (Z.equal y1 y2) then Infinity
        else
          let lam =
            if Z.equal x1 x2 then
              let num = Z.add (Z.mul (Z.of_int 3) (Z.mul x1 x1)) C.a in
              let den_inv = Z.invert (Z.mul (Z.of_int 2) y1) C.p in
              modulo (Z.mul num den_inv) C.p
            else
              let num = Z.sub y2 y1 in
              let den_inv = Z.invert (Z.sub x2 x1) C.p in
              modulo (Z.mul num den_inv) C.p
          in
          let x3 = modulo (Z.sub (Z.sub (Z.mul lam lam) x1) x2) C.p in
          let y3 = modulo (Z.sub (Z.mul lam (Z.sub x1 x3)) y1) C.p in
          Affine (x3, y3)

  let
    [@contract : any -> is_on_curve -> is_on_curve]
    scalar_mult (k : Z.t) (pt : point) : point =
    let rec loop k_rem current_pt acc =
      if Z.equal k_rem Z.zero then acc
      else
        let next_acc = if Z.testbit k_rem 0 then add_points acc current_pt else acc in
        let next_pt = add_points current_pt current_pt in
        loop (Z.shift_right k_rem 1) next_pt next_acc
    in loop k pt Infinity

  let generator = C.g
  let order = C.n
end
