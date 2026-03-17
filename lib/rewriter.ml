open Effect
open Ppxlib
open Ast_helper
open Ast_builder.Default

open Annotation
open Context

let flat_effect_name = "V"
let local_module_name = "Contract"

let pos_label = "pos"
let neg_label = "neg"
let cloc_label = "cloc"

let effect_t loc = { txt = Ldot (Lident "Effect", "t"); loc }
let string_t loc = Ast_builder.Default.ptyp_constr ~loc { txt = Lident "string"; loc } []

let build_flat_contract_wrapper ~loc (typ : core_type) (predicate : string) (expr : expression) =
  let pred_expr =
    Ast_builder.Default.pexp_ident ~loc { txt = Longident.parse predicate; loc }
  in

  (*
    struct
      type _ Effect.t += flat_effect_name : type -> type Effect.t
    end
   *)
  let contract_module =
    let params = [ (Ast_builder.Default.ptyp_any ~loc, (NoVariance, NoInjectivity)) ] in
    let effect_typ = Ast_builder.Default.ptyp_constr ~loc (effect_t loc) [typ] in
    let ext_cons =
      Ast_builder.Default.extension_constructor ~loc
        ~name:{ txt = flat_effect_name; loc }
        ~kind:(Pext_decl ([], Pcstr_tuple [typ], Some effect_typ))
    in
    let typext = Ast_builder.Default.type_extension ~loc ~path:(effect_t loc) ~params ~constructors:[ext_cons] ~private_:Public in
    Ast_builder.Default.pmod_structure ~loc [ Ast_builder.Default.pstr_typext ~loc typext ]
  in

  let handle = [%expr
    let __handler__ = {
      Effect.Deep.retc = (fun x -> x);
      exnc = (fun e -> raise e);

      effc = fun (type a) (type b) (eff : a Effect.t) ->
        match eff with
        | Contract.V arg ->
            (Some (fun (k : (a, b) Effect.Deep.continuation) ->
              if [%e pred_expr] arg then
                Effect.Deep.continue k arg
              else
                Effect.Deep.discontinue k (Blame pos)
            ) : ((a, b) Effect.Deep.continuation -> b) option)

        | _ -> None
    } in

    Effect.Deep.match_with Effect.perform (Contract.V [%e expr])
      __handler__
  ] in

  (*
    fun pos neg cloc ->
      let module Contract = struct
        type _ Effect.t += V
      end
      in
      let handler = ... in
      match_with Effect.perform (Contract.V expr) handler

      similar to "handle {handler} op_v(expr)"
   *)
  let inner_let_module =
    Ast_builder.Default.pexp_letmodule ~loc
      { txt = Some local_module_name; loc }
      contract_module
      handle
  in
  [%expr fun pos neg cloc -> [%e inner_let_module]]

let rec deep_eta_expand ~loc expr typ prefix =
  (* Helper to unroll `t1 -> t2 -> t3` into a list of arguments `[t1; t2]` and a return `t3` *)
  let rec get_args t =
    match t.ptyp_desc with
    | Ptyp_arrow (lbl, t_arg, t_ret) ->
        let args, final_ret = get_args t_ret in
        ((lbl, t_arg) :: args, final_ret)
    | _ -> ([], t)
  in

  let args, ret_typ = get_args typ in
  match args with
  | [] -> expr (* Base case: it's a flat type. No expansion needed. *)
  | _ ->
      (* 1. Build the applied arguments. If an argument is a function, recurse! *)
      let applied_args =
        List.mapi (fun i (lbl, t_arg) ->
          let arg_name = prefix ^ "_" ^ string_of_int (i + 1) in
          let arg_ident = Ast_builder.Default.pexp_ident ~loc { txt = Lident arg_name; loc } in

          (* RECURSION: Expands nested functions like g_1_1 *)
          let expanded_arg = deep_eta_expand ~loc arg_ident t_arg arg_name in

          (lbl, expanded_arg)
        ) args
      in

      (* 2. Apply the base expression to our newly expanded arguments: `g g_1` *)
      let app_expr = Ast_builder.Default.pexp_apply ~loc expr applied_args in

      (* 3. Build the parameters for the new function: `(g_1 : t1)` *)
      let params =
        List.mapi (fun i (lbl, t_arg) ->
          let arg_name = prefix ^ "_" ^ string_of_int (i + 1) in
          let inner_pat = Ast_builder.Default.ppat_var ~loc { txt = arg_name; loc } in
          let typed_pat = Ast_builder.Default.ppat_constraint ~loc inner_pat t_arg in

          { pparam_desc = Pparam_val (lbl, None, typed_pat); pparam_loc = loc }
        ) args
      in

      (* 4. Construct the function with the return constraint: `fun (...) : t2 -> ...` *)
      Ast_builder.Default.pexp_function ~loc params
        (Some (Pconstraint ret_typ))
        (Pfunction_body app_expr)

let prepend_monitor_types ~loc old_typ =
  List.fold_right (fun _ acc_typ ->
    Ast_builder.Default.ptyp_arrow ~loc Nolabel (string_t loc) acc_typ
  ) [pos_label; neg_label; cloc_label] old_typ

(* The main function to update the value_constraint option *)
let update_constraint ~loc (old_constraint : value_constraint option) : value_constraint option =
  match old_constraint with
  | Some (Pvc_constraint c) ->
      Some (Pvc_constraint { c with typ = prepend_monitor_types ~loc c.typ })

  | Some (Pvc_coercion c) ->
      Some (Pvc_coercion { c with coercion = prepend_monitor_types ~loc c.coercion })

  | None -> None

let rec extract_pattern_type (pat : pattern) : core_type option =
  match pat.ppat_desc with
  | Ppat_constraint (_inner_pat, core_typ) ->
      Some core_typ

  | Ppat_alias (inner_pat, _) ->
      extract_pattern_type inner_pat

  | _ ->
      None

let has_attribute (attr_name_to_find : string) (vb : value_binding) : bool =
  List.exists (fun attr ->
    attr.attr_name.txt = attr_name_to_find
  ) vb.pvb_attributes

let is_fixed binding =
  (* TODO: do better check, we know that contract attributes == fixed
     but we want to remove the contract attribute after rewrite
   *)
  match get_contract_payload binding.pvb_attributes with
  | Some _ -> true
  | _ -> false

let binding_name binding =
  match binding.pvb_pat.ppat_desc with
  | Ppat_var { txt = name; _ } -> Some name
  | Ppat_constraint ({ ppat_desc = Ppat_var { txt = name; _ }; _ }, _) -> Some name
  | _ -> None

let root_module_of_loc (loc : Location.t) : string =
  let fname = loc.loc_start.pos_fname in
  if fname = "" || fname = "_none_" then
    "Unknown"
  else
    fname
    |> Filename.basename
    |> Filename.remove_extension
    |> String.capitalize_ascii


let walker = object (self)
  inherit [Context.t] Ast_traverse.map_with_context as super

  method! module_binding ctx mb =
    let mod_name =
      match mb.pmb_name.txt with
      | Some name -> name
      | None -> "Anonymous"
    in
    let new_current = match ctx.current with
      | Some n -> n ^ "." ^ mod_name
      | None -> mod_name
    in
    let new_ctx = { ctx with current = Some new_current; } in
    super#module_binding new_ctx mb

  method! structure ctx items =
    (* this is the outermost items in the file
       we capture variables declared, and then going inside their expresion

       although unlikely users can declare same variable many times
       we should be aware of cases

          let [@contract] x = 1
          let x = 2
          let use_x = x

       here the x referenced in use_x will not be fixed into a function
     *)
    let (_, rewritten) = List.fold_left (fun (cctx, rewritten) item ->
      match item.pstr_desc with
      | Pstr_value (recursive, [binding]) when Option.is_some (binding_name binding) ->
          (* this is the outermost binding *)
          let var_name = Option.get (binding_name binding) in

          let module_name = match cctx.current with
            | Some n -> n
            | None -> root_module_of_loc binding.pvb_loc
          in
          let cctx = {cctx with current = Some module_name} in

          (* if recursive, then have to remember the name *)
          let new_binding = self#value_binding cctx binding in
          let new_item = {item with pstr_desc = Pstr_value (recursive, [new_binding])} in
          let new_monitor = Option.bind (binding_name new_binding) (fun name ->
            if is_fixed binding
            then Some { name = name; typ = Scope module_name; }
            else None
          )
          in

          (* remove the name from the monitor, it will be added again,
             if this variable has been fixed
           *)
          let new_monitors = List.filter (fun ({name; _}) -> name <> var_name) cctx.monitors in
          let new_monitors = match new_monitor with
            | Some m -> m :: new_monitors
            | None -> new_monitors
          in
          let new_ctx = {cctx with monitors = new_monitors} in
          (new_ctx, new_item :: rewritten)
      | _ ->
          let mapped_item = self#structure_item cctx item in
          (cctx, mapped_item :: rewritten)
    ) (ctx, []) items
    in
    List.rev rewritten

  method transform_contract_wrapper ~cloc ctx contract (vb : value_binding) =
    let loc = vb.pvb_loc in

    match vb.pvb_expr.pexp_desc with
    (* for now only transform a function annotated with [@contract]
       we require type signature, because the predicate contract will be
       attached to the type signature
       we only work for body expression, for function match cases, we ignore
    *)
    | Pexp_function (params, Some (Pconstraint rettype), Pfunction_body body) ->
        (* for each params we rewrite into below, using shadowing to bind old value
           into their contract version

           let [@contract : param_contract] p__c__ = p in
           let p = (p__c__ [@contract_arg]) in

           if p is supposedly a function then use generated argument variables
           let [@contract : k1 -> k2] p__c__ p__c__1 = p p__c__1 in

           body is wrapped into a contract as well
           let [@contract : return_contract] __ret__ = body in
           (__ret__ [@contract_ret])

           [@contract_arg] [@contract_ret] and later mentioned [@contract_dep]
           instruct the rewriter to annotate the pos/neg/cloc correctly

           !!! dependent ordering needs special care
           if the contract is a dependent contract, we must modify the return_contract
           let return_contract = return_contract (p__c__ [@contract_dep])

           ^ this should return a function, that will be applied to return value

           labels will be added correctly, and the contract is now using the new
           return_contract variable which maps the value to new dependent contract

           so that it does not check right away, although technically it still works

           labels are added in method expression
           when we go inside the body of this function with appropriate context

           we need contract_arg and contract_ret for scoping so that we can separate
           with contract that is nested inside which uses name labels, instead of pos/neg
         *)

        let rec extract_name_and_typ pat =
          match pat.ppat_desc with
          | Ppat_constraint (inner_pat, typ) ->
              (match extract_name_and_typ inner_pat with
               | Some (loc, name, _) -> Some (loc, name, Some typ)
               | None -> None)
          | Ppat_var {txt = name; _} ->
              Some (pat.ppat_loc, name, None)
          | _ -> None
        in

        let rec build_body depvar current_contract current_params acc_body =
          let wrap_argument dom rng param inner =
            let ploc = param.pparam_loc in
            let param = match param.pparam_desc with
              | Pparam_val (_, _, p) -> p
              | _ ->
                Location.raise_errorf ~loc:param.pparam_loc
                  "This parameter format is not supported"
            in
            (match extract_name_and_typ param with
             | Some (ploc, pname, Some ptype) ->
                 let pcontract_name = pname ^ "__c__" in
                 let pcontract_iden =
                   Ast_builder.Default.pexp_ident ~loc:ploc
                   { txt = Lident pcontract_name; loc = ploc }
                 in
                 let pname_iden =
                   Ast_builder.Default.pexp_ident ~loc:ploc
                   { txt = Lident pname; loc = ploc }
                 in

                 (* build the contract wrapper variable
                    let [@contract : dom] p__c__ : ptyp = pname
                    or
                    let [@contract : dom] p__c__ : ptyp =
                      fun p__c__1 p__c__2 ->
                        (p p__c__1 p__c__2)
                  *)
                 let wrapped =
                   let variable =
                     Ast_builder.Default.ppat_constraint ~loc:ploc
                        (Ast_builder.Default.ppat_var ~loc:ploc
                          { txt = pcontract_name; loc = ploc })
                        ptype
                   in
                   let expr =
                     let rec flatten_arrow t =
                       match t.ptyp_desc with
                       | Ptyp_arrow (lbl, dom, rng) ->
                           let args, ret = flatten_arrow rng in
                           ((lbl, dom) :: args, ret)
                       | _ -> ([], t)
                     in

                     let args_types, rtype = flatten_arrow ptype in
                     match args_types with
                     | [] -> pname_iden (* not a function *)
                     | _ ->
                         let params, applied_args =
                         List.mapi (fun i (lbl, arg_typ) ->
                           let arg_name = pcontract_name ^ string_of_int (i + 1) in

                           let arg_pat = Ast_builder.Default.ppat_var ~loc:ploc { txt = arg_name; loc = ploc } in
                           let typed_arg_pat = Ast_builder.Default.ppat_constraint ~loc:ploc arg_pat arg_typ in
                           let param = { pparam_desc = Pparam_val (lbl, None, typed_arg_pat); pparam_loc = ploc } in

                           (* B. Build the application argument *)
                           let arg_expr = Ast_builder.Default.pexp_ident ~loc:ploc { txt = Lident arg_name; loc = ploc } in

                           (param, (lbl, arg_expr))
                         ) args_types
                         |> List.split
                       in

                       let app_expr = Ast_builder.Default.pexp_apply ~loc:ploc
                         pname_iden applied_args
                       in

                       Ast_builder.Default.pexp_function ~loc:ploc params
                         (Some (Pconstraint rtype))
                         (Pfunction_body app_expr)
                   in
                   let attr = contract_as_attribute ~loc:cloc contract_anno dom in
                   let vb = Ast_builder.Default.value_binding ~loc:ploc
                       ~pat:variable
                       ~expr:expr
                   in
                   { vb with pvb_attributes = [attr;] }
                 in

                 Utils.debug_vb_source wrapped;

                 (* build the shadow version
                    let p = p__c__ [@contract_arg] in
                  *)
                 let shadow =
                   let variable = Ast_builder.Default.ppat_constraint ~loc:ploc
                      (Ast_builder.Default.ppat_var ~loc:ploc
                        { txt = pname; loc = ploc })
                      ptype
                   in
                   let attr = {
                     attr_name = { txt = contract_arg_anno; loc };
                     attr_payload = PStr [];
                     attr_loc = loc;
                   } in
                   Ast_builder.Default.value_binding ~loc:ploc
                     ~pat:variable
                     ~expr:{pcontract_iden with pexp_attributes = [attr;] }
                 in

                 Ast_builder.Default.pexp_let ~loc Nonrecursive [wrapped]
                   (Ast_builder.Default.pexp_let ~loc Nonrecursive [shadow] inner)

             | _ ->
                Location.raise_errorf ~loc:ploc
                  "This parameter needs a type signature for contract rewrite"
             )
          in

          match current_contract, current_params with

          | Function (dom, rng), param :: rest_params ->
              let inner = build_body None rng rest_params acc_body in
              wrap_argument dom rng param inner

          | Dependent (dom, rng), param :: rest_params ->
              let inner = build_body (Some param) rng rest_params acc_body in
              wrap_argument dom rng param inner

          | Trace (constructor, predicate), [] ->
              Location.raise_errorf ~loc:cloc
                "Trace contract is unsupported at the moment"

          | Flat final_contract, [] ->
              let ret_pat = Ast_builder.Default.ppat_var ~loc
                { txt = "__ret__"; loc } in
              let ret_pat = Ast_builder.Default.ppat_constraint ~loc
                ret_pat rettype in

              let base_ident = Ast_builder.Default.pexp_ident ~loc
                { txt = Lident "__ret__"; loc } in

              let attr = {
                attr_name = { txt = "contract"; loc };
                attr_payload = PTyp (core_type_of_contract ~loc (Flat final_contract));
                attr_loc = loc;
              } in
              let ret_attr = {
                attr_name = { txt = "contract_ret"; loc };
                attr_payload = PStr [];
                attr_loc = loc;
              } in

              let ret_vb = {
                (Ast_builder.Default.value_binding ~loc ~pat:ret_pat ~expr:acc_body)
                with pvb_attributes = [attr]
              } in

              let inner_let = Ast_builder.Default.pexp_let ~loc
                Nonrecursive [ret_vb] {base_ident with pexp_attributes = [ret_attr;]} in

              begin match depvar with
              | Some { pparam_desc = Pparam_val (_, _,
                  { ppat_desc = Ppat_constraint (
                    { ppat_desc = Ppat_var { txt = name; _ }; _ }, _); _ }); _ } ->

                let dep_attr = {
                  attr_name = { txt = contract_dep_anno; loc };
                  attr_payload = PStr [];
                  attr_loc = loc;
                } in

                let name_c_expr =
                  let ident = Ast_builder.Default.pexp_ident ~loc
                    { txt = Lident (name ^ "__c__"); loc } in
                  { ident with pexp_attributes = [dep_attr] }
                in

                let final_contract_ident = Ast_builder.Default.pexp_ident ~loc
                  { txt = Lident final_contract; loc } in
                let apply_expr = Ast_builder.Default.pexp_apply ~loc
                  final_contract_ident [(Nolabel, name_c_expr)] in

                let fc_pat = Ast_builder.Default.ppat_var ~loc
                  { txt = final_contract; loc } in

                let fc_vb = Ast_builder.Default.value_binding ~loc
                  ~pat:fc_pat ~expr:apply_expr in

                Ast_builder.Default.pexp_let ~loc Nonrecursive [fc_vb] inner_let

              | _ -> inner_let
              end

          | _, _ ->
            Location.raise_errorf ~loc:cloc
              "The contract for this function is incorrect"
        in

        let wrapped_body = build_body None contract params body in

        (* ctx must be updated to remove shadow parameter *)
        (* 1. Helper to extract names from a single pattern *)
        let rec collect_in_pattern acc pat =
          match pat.ppat_desc with
          | Ppat_var { txt; _ } -> txt :: acc
          | Ppat_constraint (p, _) -> collect_in_pattern acc p
          | _ -> acc
        in
        let bound_names =
          List.fold_left (fun acc param ->
            match param.pparam_desc with
            | Pparam_val (_label, _default_opt, pat) -> collect_in_pattern acc pat
            | Pparam_newtype _ -> acc
          ) [] params
        in

        let shadowed_ctx = {
          ctx with
          monitors = List.filter (fun m -> not (List.mem m.name bound_names)) ctx.monitors
        } in
        let wrapped_body = self#expression shadowed_ctx wrapped_body in

        (* 2. Generate the new `pos`, `neg`, `cloc` parameters *)
        let mk_extra_param name =
          let pat = Ast_builder.Default.ppat_var ~loc { txt = name; loc } in
          (* Using OCaml 5.2+ Pparam_val. If you are on 4.14, this is just (Nolabel, None, pat) *)
          { pparam_desc = Pparam_val (Nolabel, None, pat); pparam_loc = loc }
        in

        let extra_params = [
          mk_extra_param pos_label;
          mk_extra_param neg_label;
          mk_extra_param cloc_label
        ] in
        let new_params = extra_params @ params in

        let new_body =
          Ast_builder.Default.pexp_function ~loc new_params None
          (Pfunction_body wrapped_body)
        in
        { vb with
          pvb_expr = new_body;
          pvb_attributes = remove_contract_attributes vb.pvb_attributes;
          pvb_constraint = update_constraint ~loc:vb.pvb_loc vb.pvb_constraint;
        }

    | Pexp_function (_, None, _) ->
        Location.raise_errorf ~loc:vb.pvb_loc
          "Function must be annotated with type"

    | _ ->
        let new_expr = self#expression ctx vb.pvb_expr in
        match (vb.pvb_constraint, contract) with
        | (Some (Pvc_constraint { typ; _ }), Flat predicate) ->
            { vb with
              pvb_expr = build_flat_contract_wrapper ~loc:vb.pvb_loc typ predicate new_expr;
              pvb_attributes = remove_contract_attributes vb.pvb_attributes;
              pvb_constraint = update_constraint ~loc:vb.pvb_loc vb.pvb_constraint;
            }
        | (_, Flat predicate) ->
            Location.raise_errorf ~loc:cloc
              "contract expression needs value's type annotation"
        | _ ->
            Utils.debug_vb_source vb;
            Location.raise_errorf ~loc:cloc
              "Invalid expression to convert into contract"

  (* don't modify the context here, ctx should be modified somewhere else *)
  method! value_binding ctx vb =
    let contract_attr = get_contract_attr vb.pvb_attributes in
    match contract_attr with
    | Some { attr_payload = PTyp t; attr_loc; _ } ->
        let contract = parse_contract_annotations t in
        (match contract with
        | Some contract ->
            self#transform_contract_wrapper ~cloc:attr_loc ctx contract vb
        | _ ->
          Location.raise_errorf ~loc:attr_loc "Contract cannot be parsed correctly")

    | Some { attr_loc; _ } ->
        Location.raise_errorf ~loc:attr_loc
          "Contract must be a type expression like [@contract : k1 -> k2]"

    | _ ->
        let new_expr = self#expression ctx vb.pvb_expr in
        { vb with pvb_expr = new_expr }

  method! expression ctx expr =
    match expr.pexp_desc with
    | Pexp_let (rec_flag, bindings, body) ->
        let (new_ctx, new_bindings) = List.fold_left (fun (cctx, new_bindings) binding ->
          let new_binding = self#value_binding cctx binding in
          let var_name = (binding_name new_binding) in
          let new_monitor = Option.bind var_name (fun name ->
            None
            (* if is_fixed binding
            then Some { name; typ = Nested (Option.value ~default:"" cctx.current) }
            else None *)
          )
          in

          let new_monitors = List.filter (fun ({name; _}) -> name <> (Option.get var_name)) cctx.monitors in
          let new_monitors = match new_monitor with
          | Some m -> m :: new_monitors
          | None -> new_monitors
          in
          (* remove duplicated name because shadowing *)
          ({cctx with monitors = new_monitors}, new_binding :: new_bindings)
        ) (ctx, []) bindings in

        let mapped_body = self#expression new_ctx body in

        (* Printf.eprintf "fix using context %s expr %s\n" (show_rewrite_context ctx) (Pprintast.string_of_expression body); *)

        { expr with pexp_desc = Pexp_let (rec_flag, List.rev new_bindings, mapped_body) }

    | Pexp_ident { txt = Lident var_name; loc } ->
        let loc = expr.pexp_loc in

        let contract_kind =
          List.find_map (fun attr ->
            match attr.attr_name.txt with
            | contract_arg_anno -> Some contract_arg_anno
            | contract_dep_anno -> Some contract_dep_anno
            | contract_ret_anno -> Some contract_ret_anno
            | _ -> None
          ) expr.pexp_attributes
        in

        begin match contract_kind with
        | Some kind ->
            let clean_attrs = remove_contract_attributes expr.pexp_attributes in
            let clean_expr = { expr with pexp_attributes = clean_attrs } in

            let mk_ident name = Ast_builder.Default.pexp_ident ~loc
              { txt = Lident name; loc } in

            let arg_a, arg_b, arg_c =
              match kind with
              | k when k = contract_arg_anno
                -> (mk_ident "neg", mk_ident "pos", mk_ident "cloc")
              | k when k = contract_dep_anno
                -> (mk_ident "neg", mk_ident "cloc", mk_ident "cloc")
              | k when k = contract_ret_anno
                -> (mk_ident "pos", mk_ident "neg", mk_ident "cloc")
              | _ ->
                Location.raise_errorf ~loc "Annotation?"
            in

            let args_list = [
              (Nolabel, arg_a);
              (Nolabel, arg_b);
              (Nolabel, arg_c);
            ] in
            Ast_builder.Default.pexp_apply ~loc clean_expr args_list

        | None ->
            let monitored = List.find_opt (fun { name; _ } -> name = var_name) ctx.monitors in
            begin match monitored with
            | Some { name; typ = Scope m; } ->
                let arg_a = Ast_builder.Default.estring ~loc m in
                let arg_b = Ast_builder.Default.pexp_ident ~loc { txt = Lident "__FUNCTION__"; loc } in
                let arg_c = Ast_builder.Default.estring ~loc (m ^ "." ^ name) in

                let args_list = [
                  (Nolabel, arg_a);
                  (Nolabel, arg_b);
                  (Nolabel, arg_c);
                ] in
                Ast_builder.Default.pexp_apply ~loc expr args_list

            | Some { name; typ = Nested parent; } ->
                Location.raise_errorf ~loc "Unsupported nested contract"

            | _ ->
                super#expression ctx expr
            end
        end

    | _ -> super#expression ctx expr
end
