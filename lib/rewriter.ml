(* Surface forms handled by the ppx:

   [%mon c ~pos:"k" ~neg:"l" ~loc:"j"] e
       The formal (mon k l j c e). Labels are optional.

   let[@contract c] x = e      (also let rec, local lets, nested modules)
       x is monitored with pos = loc = the qualified name of x. Each use of x
       in the same compilation unit becomes a monitor whose neg label is the
       definition that contains the use, so clients are blamed precisely.
       At the top level of a module, x is also exported with neg "external".

   [%hijack] e  /  [%hijack policy] e
       Run e under a custom handler for contract checks (C-Hijack of
       redex/compiler3.rkt). Without a policy, checks are skipped. *)

open Ppxlib
open Ast_builder.Default

type env = {
  root : string;                      (* compilation unit name *)
  path : string list;                 (* enclosing modules/definitions, innermost first *)
  protected : (string * string) list; (* contracted name -> its monitor function *)
}

let empty = { root = ""; path = []; protected = [] }

let root_of_loc (loc : Location.t) =
  match loc.loc_start.pos_fname with
  | "" | "_none_" -> "Unknown"
  | f -> f |> Filename.basename |> Filename.remove_extension |> String.capitalize_ascii

let label env = String.concat "." (env.root :: List.rev env.path)

let enter name env = { env with path = name :: env.path }

let bound_vars =
  object
    inherit [string list] Ast_traverse.fold as super

    method! pattern p acc =
      let acc = super#pattern p acc in
      match p.ppat_desc with
      | Ppat_var { txt; _ } | Ppat_alias (_, { txt; _ }) -> txt :: acc
      | _ -> acc
  end

let unprotect names env =
  { env with protected = List.filter (fun (x, _) -> not (List.mem x names)) env.protected }

let unprotect_pat p env = unprotect (bound_vars#pattern p []) env

let contract_of_vb vb =
  List.find_map
    (fun a ->
      if a.attr_name.txt = "contract" then Some (Syntax.of_payload ~loc:a.attr_loc a.attr_payload)
      else None)
    vb.pvb_attributes

let without_contract_attr vb =
  { vb with pvb_attributes = List.filter (fun a -> a.attr_name.txt <> "contract") vb.pvb_attributes }

let rec var_of_pat p =
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> Some txt
  | Ppat_constraint (p, _) -> var_of_pat p
  | _ -> None

let rec rename_pat p name =
  match p.ppat_desc with
  | Ppat_var v -> { p with ppat_desc = Ppat_var { v with txt = name } }
  | Ppat_constraint (q, t) -> { p with ppat_desc = Ppat_constraint (rename_pat q name, t) }
  | _ -> p

let estring ~loc s = pexp_constant ~loc (Pconst_string (s, loc, None))

(* A contracted binding x = e becomes
     raw  = e
     monx = fun neg -> mon "path.x" neg "path.x" c raw *)
type contracted = {
  name : string;
  raw : string;
  monf : string;
  contract : Syntax.t;
  vb : value_binding;
}

let analyse vb =
  match contract_of_vb vb with
  | None -> None
  | Some contract -> (
      match var_of_pat vb.pvb_pat with
      | None ->
          Location.raise_errorf ~loc:vb.pvb_loc
            "[@contract] can only annotate a binding of a single variable"
      | Some name ->
          Some
            { name;
              raw = Codegen.gensym ("raw_" ^ name ^ "_");
              monf = Codegen.gensym ("mon_" ^ name ^ "_");
              contract;
              vb = without_contract_attr vb })

let use_site ~loc env monf = Codegen.app ~loc (Codegen.evar ~loc monf) [ estring ~loc (label env) ]

let split_labels (e : expression) =
  match e.pexp_desc with
  | Pexp_apply (f, args) ->
      let is_label = function
        | Labelled ("pos" | "neg" | "loc"), _ -> true
        | _ -> false
      in
      let labels, rest = List.partition is_label args in
      let find l = List.find_map (function Labelled l', v when l = l' -> Some v | _ -> None) labels in
      let c = if rest = [] then f else { e with pexp_desc = Pexp_apply (f, rest) } in
      (c, find "pos", find "neg", find "loc")
  | _ -> (e, None, None, None)

let loc_label (loc : Location.t) =
  Printf.sprintf "%s:%d" (Filename.basename loc.loc_start.pos_fname) loc.loc_start.pos_lnum

let rewriter =
  object (self)
    inherit [env] Ast_traverse.map_with_context as super

    (* names bound by a group of bindings, and the env their bodies see *)
    method private rhs_env env rec_flag (items : (contracted option * value_binding) list) =
      let plain = List.filter_map (fun (c, vb) -> if c = None then Some vb.pvb_pat else None) items in
      let plain_names = List.concat_map (fun p -> bound_vars#pattern p []) plain in
      match rec_flag with
      | Nonrecursive -> env
      | Recursive ->
          let env = unprotect plain_names env in
          let added = List.filter_map (fun (c, _) -> Option.map (fun c -> (c.name, c.monf)) c) items in
          { env with protected = added @ env.protected }

    method private after_env env (items : (contracted option * value_binding) list) =
      List.fold_left
        (fun env (c, vb) ->
          match c with
          | Some c -> { (unprotect [ c.name ] env) with protected = (c.name, c.monf) :: env.protected }
          | None -> unprotect_pat vb.pvb_pat env)
        env items

    (* the bindings replacing a group: raw values (and plain bindings) in
       the first group, monitor functions in the second *)
    method private expand env rec_flag vbs =
      let items = List.map (fun vb -> (analyse vb, vb)) vbs in
      let renv = self#rhs_env env rec_flag items in
      let map_rhs vb =
        let env' = match var_of_pat vb.pvb_pat with Some n -> enter n renv | None -> renv in
        self#expression env' vb.pvb_expr
      in
      let values =
        List.map
          (fun (c, vb) ->
            match c with
            | Some c -> { c.vb with pvb_pat = rename_pat c.vb.pvb_pat c.raw; pvb_expr = map_rhs c.vb }
            | None -> self#value_binding renv vb)
          items
      in
      let monitors =
        List.filter_map
          (fun (c, _) ->
            Option.map
              (fun c ->
                let loc = c.vb.pvb_loc in
                let me = label (enter c.name env) in
                let neg = Codegen.gensym "neg" in
                let body =
                  Codegen.monitor ~loc ~pos:(estring ~loc me) ~neg:(Codegen.evar ~loc neg)
                    ~cl:(estring ~loc me) c.contract (Codegen.evar ~loc c.raw)
                in
                value_binding ~loc ~pat:(Codegen.pvar ~loc c.monf) ~expr:(Codegen.fun_ ~loc neg body))
              c)
          items
      in
      let contracted = List.filter_map fst items in
      (items, values, monitors, contracted)

    method! structure env items =
      let env =
        if env.root = "" then
          match items with
          | i :: _ -> { env with root = root_of_loc i.pstr_loc }
          | [] -> env
        else env
      in
      let _, out =
        List.fold_left
          (fun (env, acc) item ->
            match item.pstr_desc with
            | Pstr_value (rec_flag, vbs) when List.exists (fun vb -> contract_of_vb vb <> None) vbs ->
                let loc = item.pstr_loc in
                let items, values, monitors, contracted = self#expand env rec_flag vbs in
                let defs =
                  match rec_flag with
                  | Recursive -> [ pstr_value ~loc Recursive (values @ monitors) ]
                  | Nonrecursive ->
                      [ pstr_value ~loc Nonrecursive values; pstr_value ~loc Nonrecursive monitors ]
                in
                (* exported under the original name, for other modules *)
                let exports =
                  List.map
                    (fun c ->
                      let use = Codegen.app ~loc (Codegen.evar ~loc c.monf) [ estring ~loc "external" ] in
                      let expr =
                        if Codegen.is_function_contract c.contract then
                          let a = Codegen.gensym "a" in
                          Codegen.fun_ ~loc a (Codegen.app ~loc use [ Codegen.evar ~loc a ])
                        else use
                      in
                      let vb = value_binding ~loc ~pat:(Codegen.pvar ~loc c.name) ~expr in
                      let unused_ok =
                        attribute ~loc ~name:{ txt = "warning"; loc }
                          ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-32") [] ])
                      in
                      pstr_value ~loc Nonrecursive [ { vb with pvb_attributes = [ unused_ok ] } ])
                    contracted
                in
                (self#after_env env items, List.rev_append (defs @ exports) acc)
            | Pstr_value (rec_flag, vbs) ->
                let items = List.map (fun vb -> (None, vb)) vbs in
                let renv = self#rhs_env env rec_flag items in
                let vbs = List.map (self#value_binding renv) vbs in
                (self#after_env env items, { item with pstr_desc = Pstr_value (rec_flag, vbs) } :: acc)
            | _ -> (env, self#structure_item env item :: acc))
          (env, []) items
      in
      List.rev out

    method! module_binding env mb =
      let name = Option.value mb.pmb_name.txt ~default:"_" in
      super#module_binding (enter name env) mb

    method! value_binding env vb =
      let env = match var_of_pat vb.pvb_pat with Some n -> enter n env | None -> env in
      super#value_binding env vb

    method! case env c = super#case (unprotect_pat c.pc_lhs env) c

    method! expression env e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      | Pexp_ident { txt = Lident x; _ } when List.mem_assoc x env.protected ->
          use_site ~loc env (List.assoc x env.protected)
      | Pexp_let (rec_flag, vbs, body) when List.exists (fun vb -> contract_of_vb vb <> None) vbs ->
          let items, values, monitors, _ = self#expand env rec_flag vbs in
          let body = self#expression (self#after_env env items) body in
          (match rec_flag with
           | Recursive -> pexp_let ~loc Recursive (values @ monitors) body
           | Nonrecursive -> pexp_let ~loc Nonrecursive values (pexp_let ~loc Nonrecursive monitors body))
      | Pexp_let (rec_flag, vbs, body) ->
          let items = List.map (fun vb -> (None, vb)) vbs in
          let renv = self#rhs_env env rec_flag items in
          let vbs = List.map (self#value_binding renv) vbs in
          { e with pexp_desc = Pexp_let (rec_flag, vbs, self#expression (self#after_env env items) body) }
      | Pexp_function (params, constr, body) ->
          let env, params =
            List.fold_left
              (fun (env, ps) p ->
                match p.pparam_desc with
                | Pparam_val (l, def, pat) ->
                    let def = Option.map (self#expression env) def in
                    (unprotect_pat pat env, { p with pparam_desc = Pparam_val (l, def, pat) } :: ps)
                | Pparam_newtype _ -> (env, p :: ps))
              (env, []) params
          in
          let body =
            match body with
            | Pfunction_body b -> Pfunction_body (self#expression env b)
            | Pfunction_cases (cs, l, a) -> Pfunction_cases (List.map (self#case env) cs, l, a)
          in
          { e with pexp_desc = Pexp_function (List.rev params, constr, body) }
      | Pexp_for (pat, lo, hi, dir, body) ->
          let benv = unprotect_pat pat env in
          { e with
            pexp_desc =
              Pexp_for (pat, self#expression env lo, self#expression env hi, dir, self#expression benv body) }
      (* [%mon c ~pos ~neg ~loc] e *)
      | Pexp_apply ({ pexp_desc = Pexp_extension ({ txt = "mon"; _ }, payload); _ }, [ (Nolabel, arg) ]) ->
          let c, pos, neg, cl =
            match payload with
            | PStr [ { pstr_desc = Pstr_eval (c, _); _ } ] -> split_labels c
            | _ -> Location.raise_errorf ~loc "[%%mon c] expects a contract"
          in
          let c = Syntax.of_expression (self#expression env c) in
          let lab d = function Some l -> self#expression env l | None -> estring ~loc d in
          Codegen.monitor ~loc ~pos:(lab "server" pos) ~neg:(lab "client" neg)
            ~cl:(lab (loc_label loc) cl) c (self#expression env arg)
      | Pexp_extension ({ txt = "mon"; _ }, _) ->
          Location.raise_errorf ~loc "[%%mon c] must be applied to an expression: [%%mon c] e"
      (* [%hijack] e / [%hijack policy] e *)
      | Pexp_apply ({ pexp_desc = Pexp_extension ({ txt = "hijack"; _ }, payload); _ }, [ (Nolabel, arg) ]) ->
          let policy =
            match payload with
            | PStr [] -> Codegen.runtime ~loc "Contract" "unchecked"
            | PStr [ { pstr_desc = Pstr_eval (p, _); _ } ] -> self#expression env p
            | _ -> Location.raise_errorf ~loc "[%%hijack policy] expects a policy expression"
          in
          Codegen.app ~loc (Codegen.runtime ~loc "Contract" "with_policy")
            [ policy; pexp_fun ~loc Nolabel None (punit ~loc) (self#expression env arg) ]
      | Pexp_extension ({ txt = "hijack"; _ }, _) ->
          Location.raise_errorf ~loc "[%%hijack] must be applied to an expression: [%%hijack] e"
      | _ -> super#expression env e
  end
