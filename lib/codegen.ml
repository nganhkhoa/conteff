(* Compiling (mon k l j κ e), following the `compile` judgment of
   redex/compiler.rkt. Labels: pos = k (the party providing the value),
   neg = l (its context), cl = j (the contract location).

   Flat contracts become a [Check] perform (C-Mon-Flat) and nothing else: no
   handler is installed here. Function contracts are padded with wrapper
   lambdas that re-monitor the argument with swapped labels and the result
   (C-Mon-Func, C-Mon-Dep). Tuples are split (C-Mon-Tuple). The handler for
   [Check] lives outside, so the context decides what a check means. *)

open Ppxlib
open Ast_builder.Default

let counter = ref 0

let gensym hint =
  incr counter;
  Printf.sprintf "__conteff_%s%d" hint !counter

let runtime ~loc m name =
  pexp_ident ~loc { txt = Ldot (Ldot (Lident "Conteff_lib", m), name); loc }

let evar ~loc x = pexp_ident ~loc { txt = Lident x; loc }

let pvar ~loc x = ppat_var ~loc { txt = x; loc }

let let_ ~loc pat rhs body =
  pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:rhs ] body

let fun_ ~loc x body = pexp_fun ~loc Nolabel None (pvar ~loc x) body

let app ~loc f args = pexp_apply ~loc f (List.map (fun a -> (Nolabel, a)) args)

(* Evaluate contract expressions first: predicates and custom projections are
   bound once before the monitored expression, preventing variable capture.
   Expressions inside a dependent range are bound later, under its binder. *)
let rec hoist (c : Syntax.t) : (string * expression) list * Syntax.t =
  match c with
  | Flat ({ pexp_desc = Pexp_extension ({ txt; _ },
        PStr [{ pstr_desc = Pstr_eval (projection, _); _ }]); _ } as e)
      when String.equal txt Syntax.custom_marker ->
      let x = gensym "projection" in
      ([ (x, projection) ], Syntax.custom ~loc:e.pexp_loc (evar ~loc:e.pexp_loc x))
  | Flat e ->
      let x = gensym "pred" in
      ([ (x, e) ], Flat (evar ~loc:e.pexp_loc x))
  | Fun (a, b) ->
      let ba, a = hoist a in
      let bb, b = hoist b in
      (ba @ bb, Fun (a, b))
  | Dep (a, p, b) ->
      let ba, a = hoist a in
      (ba, Dep (a, p, b))
  | Tuple cs ->
      let bs, cs = List.split (List.map hoist cs) in
      (List.concat bs, Tuple cs)
  | Box c ->
      let b, c = hoist c in
      (b, Box c)
  | Vector c ->
      let b, c = hoist c in
      (b, Vector c)

let bind_all ~loc binds body =
  List.fold_right (fun (x, e) acc -> let_ ~loc (pvar ~loc x) e acc) binds body

let rec mon ~loc ~pos ~neg ~cl (c : Syntax.t) (e : expression) : expression =
  match c with
  | Flat { pexp_desc = Pexp_extension ({ txt; _ },
        PStr [{ pstr_desc = Pstr_eval (projection, _); _ }]); _ }
      when String.equal txt Syntax.custom_marker ->
      pexp_apply ~loc projection
        [ (Labelled "pos", pos); (Labelled "neg", neg); (Labelled "loc", cl); (Nolabel, e) ]
  (* C-Mon-Flat: (perform 𝒞) (pred, e) *)
  | Flat pred ->
      pexp_apply ~loc (runtime ~loc "Contract" "check")
        [ (Labelled "pos", pos); (Labelled "loc", cl); (Nolabel, pred); (Nolabel, e) ]
  (* C-Mon-Func: (λf. λx. mon k l j κ2 (f (mon l k j κ1 x))) e *)
  | Fun (dom, rng) ->
      let f = gensym "f" and x = gensym "x" in
      let arg = mon ~loc ~pos:neg ~neg:pos ~cl dom (evar ~loc x) in
      let_ ~loc (pvar ~loc f) e
        (fun_ ~loc x (mon ~loc ~pos ~neg ~cl rng (app ~loc (evar ~loc f) [ arg ])))
  (* C-Mon-Dep:
       (λf. λx. (λy. mon k l j κ2 (f (mon l k j κ1 x))) (mon l j j κ1 x)) e *)
  | Dep (dom, y, rng) ->
      let f = gensym "f" and x = gensym "x" in
      let dep_arg = mon ~loc ~pos:neg ~neg:cl ~cl dom (evar ~loc x) in
      let arg = mon ~loc ~pos:neg ~neg:pos ~cl dom (evar ~loc x) in
      let binds, rng = hoist rng in
      let_ ~loc (pvar ~loc f) e
        (fun_ ~loc x
           (let_ ~loc y dep_arg
              (bind_all ~loc binds
                 (mon ~loc ~pos ~neg ~cl rng (app ~loc (evar ~loc f) [ arg ])))))
  (* C-Mon-Tuple: monitor each component, left to right *)
  | Tuple cs ->
      let vs = List.map (fun _ -> gensym "v") cs in
      let rs = List.map (fun _ -> gensym "r") cs in
      let tuple xs = pexp_tuple ~loc (List.map (evar ~loc) xs) in
      let body =
        List.fold_right2
          (fun (c, v) r acc -> let_ ~loc (pvar ~loc r) (mon ~loc ~pos ~neg ~cl c (evar ~loc v)) acc)
          (List.combine cs vs) rs (tuple rs)
      in
      let_ ~loc (ppat_tuple ~loc (List.map (pvar ~loc) vs)) e body
  (* A box is its pair of operations: reads are monitored like results
     (unit -> κ), writes like arguments (κ -> unit). *)
  | Box c ->
      let b = gensym "box" and v = gensym "v" in
      let get = app ~loc (runtime ~loc "Box" "get") [ evar ~loc b ] in
      let set w = app ~loc (runtime ~loc "Box" "set") [ evar ~loc b; w ] in
      let_ ~loc (pvar ~loc b) e
        (pexp_apply ~loc (runtime ~loc "Box" "make")
           [ (Labelled "get",
              pexp_fun ~loc Nolabel None (punit ~loc) (mon ~loc ~pos ~neg ~cl c get));
             (Labelled "set",
              fun_ ~loc v (set (mon ~loc ~pos:neg ~neg:pos ~cl c (evar ~loc v)))) ])
  | Vector c ->
      let b = gensym "vec" and i = gensym "i" and v = gensym "v" in
      let get = app ~loc (runtime ~loc "Vector" "get") [ evar ~loc b; evar ~loc i ] in
      let set w = app ~loc (runtime ~loc "Vector" "set") [ evar ~loc b; evar ~loc i; w ] in
      let_ ~loc (pvar ~loc b) e
        (pexp_apply ~loc (runtime ~loc "Vector" "make")
           [ (Labelled "length", app ~loc (runtime ~loc "Vector" "length") [ evar ~loc b ]);
             (Labelled "get", fun_ ~loc i (mon ~loc ~pos ~neg ~cl c get));
             (Labelled "set",
              fun_ ~loc i (fun_ ~loc v (set (mon ~loc ~pos:neg ~neg:pos ~cl c (evar ~loc v))))) ])

(* Entry point: (mon pos neg cl c e) with the contract's predicates
   evaluated first. *)
let monitor ~loc ~pos ~neg ~cl (c : Syntax.t) (e : expression) : expression =
  let binds, c = hoist c in
  bind_all ~loc binds (mon ~loc ~pos ~neg ~cl c e)

let is_function_contract : Syntax.t -> bool = function
  | Fun _ | Dep _ -> true
  | _ -> false

(* A locally abstract result type is essential: matching an extensible effect
   GADT refines the result type for this particular constructor. Parse only a
   fixed skeleton; splice the original predicate expressions into its AST. *)
let effect_monitor ~loc ~pos ~neg ~cl (c : Syntax.effect_contract) f =
  let request = gensym "effect_request" and response = gensym "effect_response" in
  let interceptor = gensym "intercept" and payload = gensym "payload" in
  let operation = Format.asprintf "%a" Pprintast.longident c.operation.txt in
  let source = Printf.sprintf
    "let %s : type a. a Effect.t -> (unit -> a) option = function \
     | %s %s -> Some (fun () -> \
       Conteff_lib.Contract.forward_effect ~pos:__conteff_pos_marker \
       ~neg:__conteff_neg_marker ~loc:__conteff_loc_marker \
       ~request:%s ~response:%s \
       (fun x -> Effect.perform (%s x)) %s) \
     | _ -> None in \
     Conteff_lib.Contract.effectful_function \
       { Conteff_lib.Contract.intercept = %s } __conteff_fun_marker"
    interceptor operation payload request response operation payload interceptor in
  let lexbuf = Lexing.from_string source in
  let template = Parse.expression lexbuf in
  let mapper = object
    inherit Ast_traverse.map as super
    method! expression e =
      match e.pexp_desc with
      | Pexp_ident { txt = Lident "__conteff_pos_marker"; _ } -> pos
      | Pexp_ident { txt = Lident "__conteff_neg_marker"; _ } -> neg
      | Pexp_ident { txt = Lident "__conteff_loc_marker"; _ } -> cl
      | Pexp_ident { txt = Lident "__conteff_fun_marker"; _ } -> f
      | _ -> super#expression e
  end in
  bind_all ~loc [request, c.request; response, c.response] (mapper#expression template)
