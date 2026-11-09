open Ast
open East
open Mapper
open Emapper

(* Validation Pass for Typed System is built different
   Two things to note here

   1. Assignment suggests building a way to track errors
   2. Type checking goes from inside to outside

   To do this, we consider building an extended AST
   this AST has "flags" for errors tracking

   Also, to walk this AST, we use the same Functor style
   mapping technique, but instead of handling Enter
   we handle Exit, making sure that the inner most nodes
   are processed ("tracked")

   Because traversal produces a different tree, we can split
   the whole process into two,

   1. a outer-to-inner pass to validate trivial errors
   2. a inner-to-outer pass to validate type errors
 *)

type type_error =
  | IllTyped
  | UnknownModule of string
  | DuplicateModule
  | MethodReturnTypeWrong
  | ClassMalformed
  | UnknownField
  | UnknownMethod
  | VariableNotFound of string
  | ImportMixing
  [@@deriving show, eq]

type deduced_type =
  | Empty
  [@@deriving show, eq]

type typed_info = {
  error: type_error option;
}
[@@deriving show]

module Info = struct
  type t = typed_info
  let default = {
    error = None;
  }
end

module MapperConfig = struct
  type info = typed_info

  type e_scope =
    | ScopeEModule of (enter_type * info e_module)
    | ScopeEImport of (enter_type * info e_import)
    | ScopeEClass of (enter_type * info e_classdef)
    | ScopeEMethod of (enter_type * info e_classmethod)
    | ScopeEDef of (enter_type * info e_declaration)
    | ScopeESystem of (enter_type * info e_system)
    | ScopeEExpr of (enter_type * info e_expression)
    | ScopeEStmt of (enter_type * info e_statement)
    | ScopeEBlock of (enter_type * info e_block)

  type _ Effect.t +=
    (* Enter for outer-to-inner handlers
       Returns the new info type
     *)
    | EnterEModule : (enter_type * info e_module) -> info Effect.t
    | EnterEImport : (enter_type * info e_import) -> info Effect.t
    | EnterEClass  : (enter_type * info e_classdef) -> info Effect.t
    | EnterEMethod : (enter_type * info e_classmethod) -> info Effect.t
    | EnterEDef    : (enter_type * info e_declaration) -> info Effect.t
    | EnterESystem : (enter_type * info e_system) -> info Effect.t
    | EnterEExpr   : (enter_type * info e_expression) -> info Effect.t
    | EnterEStmt   : (enter_type * info e_statement) -> info Effect.t
    | EnterEBlock  : (enter_type * info e_block) -> info Effect.t

  let map_variable v = v
  let map_classname c = c
  let map_classfield f = f
  let map_methodparam p = p

  let enter_scope = function
    | ScopeESystem s -> Effect.perform (EnterESystem s)
    | ScopeEModule m -> Effect.perform (EnterEModule m)
    | ScopeEImport i -> Effect.perform (EnterEImport i)
    | ScopeEClass c -> Effect.perform (EnterEClass c)
    | ScopeEMethod m -> Effect.perform (EnterEMethod m)
    | ScopeEDef d -> Effect.perform (EnterEDef d)
    | ScopeEExpr e -> Effect.perform (EnterEExpr e)
    | ScopeEStmt s -> Effect.perform (EnterEStmt s)
    | ScopeEBlock b -> Effect.perform (EnterEBlock b)
end
module TypedValidatorRunner = MakeExtendedMapper(MapperConfig)

(* we compose the rules
    something |- something-else
   for rules that computes . |- . => .
      make functions that returns result
   for rules that checks . |- .
      checks and returns check error or none
   because we need to update the nodes, new nodes are returned
   with their info updated
 *)

(* don't use hashmap for complexity *)
type classmapping = (classname * shape option) list
type varmapping = (name * typ) list

let rec has_duplicated names =
  match names with
  | [] -> false
  | hd :: tail ->
      if List.mem hd tail then
        true
      else
        has_duplicated tail

let rec c_expr sclasses vars expr =
  match expr.node with
  | ENum n -> expr
  | EVar var ->
    if List.mem var vars
    then expr
    else {expr with info = {error = Some (VariableNotFound var)}}
  | EAdd (lhs, rhs) ->
      (
        match (List.mem lhs vars, List.mem rhs vars) with
      | (true, true) -> expr
      | (true, _) -> {expr with info = {error = Some (VariableNotFound rhs)}}
      | _ -> {expr with info = {error = Some (VariableNotFound lhs)}}
    )
  | EDiv (lhs, rhs) ->
      (
        match (List.mem lhs vars, List.mem rhs vars) with
      | (true, true) -> expr
      | (true, _) -> {expr with info = {error = Some (VariableNotFound rhs)}}
      | _ -> {expr with info = {error = Some (VariableNotFound lhs)}}
    )
  | EEq (lhs, rhs) ->
      (
        match (List.mem lhs vars, List.mem rhs vars) with
      | (true, true) -> expr
      | (true, _) -> {expr with info = {error = Some (VariableNotFound rhs)}}
      | _ -> {expr with info = {error = Some (VariableNotFound lhs)}}
    )
  | ENewClass (classname, fieldvars) ->
    (
      match List.assoc_opt classname sclasses with
      | Some _ ->
        let undeclared_var = List.find_opt (fun fieldvar ->
          not (List.mem fieldvar vars)
          ) fieldvars
        in
        (
          match undeclared_var with
          | None -> expr
          | Some v -> {expr with info = {error = Some (VariableNotFound v)}}
        )
      | _ -> {expr with info = {error = Some (VariableNotFound classname)}}
    )
  | EFieldAccess (lhs, rhs) ->
    if List.mem lhs vars
    then expr
    else {expr with info = {error = Some (VariableNotFound lhs)}}
  | EMethodCall (var, methname, params) ->
    if List.mem var vars
    then
        let undeclared_var = List.find_opt (fun var ->
          not (List.mem var vars)
          ) params
        in
        (
          match undeclared_var with
          | None -> expr
          | Some v -> {expr with info = {error = Some (VariableNotFound v)}}
        )
    else {expr with info = {error = Some (VariableNotFound var)}}
  | EIsA (var, classname) ->
    if List.mem var vars
    then
      match List.assoc_opt classname sclasses with
      | Some _ ->  expr
      | None -> {expr with info = {error = Some (VariableNotFound classname)}}
    else {expr with info = {error = Some (VariableNotFound var)}}

and c_block sclasses vars block =
  (
    match block.node with
    | ESimple stmt ->
      let new_stmt = c_stmt sclasses vars stmt in
      {block with node = ESimple new_stmt}
    | EComplex (decls, stmts) ->
      (* check decls *)
      let (inner_vars, new_defs) = List.fold_left (fun (accum_vars, new_defs) def ->
        let new_def = c_def sclasses accum_vars def in
        (def.node.var :: accum_vars, new_def :: new_defs)
        ) (vars, []) decls
      in

      (* check stmts *)
      let new_stmts = List.map (fun stmt -> c_stmt sclasses inner_vars stmt) stmts in

      {block with node = EComplex (new_defs, new_stmts)}
  )

and c_stmt sclasses vars stmt =
  match stmt.node with
  | EAssign (var, expr) ->
    let new_expr = c_expr sclasses vars expr in
    (
      match List.find_opt (fun v -> v = var) vars with
      | Some _ -> {stmt with node = EAssign (var, new_expr)}
      | _ -> {node = EAssign (var, new_expr); info = {error = Some (VariableNotFound var)}}
    )
  | EFieldAssign (var, fieldname, expr) ->
    let new_expr = c_expr sclasses vars expr in
    (
      match List.find_opt (fun v -> v = var) vars with
      | Some _ -> {stmt with node = EFieldAssign (var, fieldname,  new_expr)}
      | _ -> {node = EFieldAssign (var, fieldname, new_expr); info = {error = Some (VariableNotFound var)}}
    )
  | EIf0 (expr, thn_block, els_block) ->
    let new_expr = c_expr sclasses vars expr in
    let new_thn_block = c_block sclasses vars thn_block in
    let new_els_block = c_block sclasses vars els_block in
    {stmt with node = EIf0 (new_expr, new_thn_block, new_els_block)}
  | EWhile0 (expr, block) ->
    let new_expr = c_expr sclasses vars expr in
    let new_block = c_block sclasses vars block in
    {stmt with node = EWhile0 (new_expr, new_block)}

and c_def sclasses vars def =
  let { var; value } = def.node in
  let new_value = c_expr sclasses vars value in
  {def with node = { var; value = new_value; } }


let c_method sclasses meth =
  let { name; params; defs; stmts; body } = meth.node in
  (* check params *)
  let param_duplicated = has_duplicated params in

  (* check defs *)
  let (vars, new_defs) = List.fold_left (fun (accum_vars, new_defs) def ->
    let new_def = c_def sclasses accum_vars def in
    (def.node.var :: accum_vars, new_def :: new_defs)
    ) ("this" :: params, []) defs
  in

  (* check stmts *)
  let new_stmts = List.map (fun stmt -> c_stmt sclasses vars stmt) stmts in

  (* check body *)
  let new_body = c_expr sclasses vars body in

  if param_duplicated
  then
  {node = { name; params; defs = new_defs; stmts = new_stmts; body = new_body; };
    info = {error = Some ClassMalformed }}
  else
  { meth with node = { name; params; defs = new_defs; stmts = new_stmts; body = new_body; }}

(* untyped error checking *)
let utc_class
  (sclasses : classmapping)
  (cls : typed_info e_classdef)
  =
  let { node = { name; fields; methods }; info = _ }  = cls in
  (* checking an untyped class *)
  let field_duplicated = has_duplicated fields in

  let method_names = List.map (fun (meth : ('a classmethod_node, typed_info) ic) ->
    meth.node.name
    ) methods
  in
  let method_duplicated = has_duplicated method_names in
  let new_methods = List.map (c_method sclasses) methods in

  if field_duplicated || method_duplicated (*|| method_param_duplicated*)
  then {cls with info = { error = Some ClassMalformed } }
  else {cls with node = { name; fields; methods = new_methods; } }


let rec type_eq dis dat =
  match (dis, dat) with
  | Real, Real -> true
  | Empty, Empty -> true
  | Shape (dis_fields, dis_methods), Shape (dat_fields, dat_methods)
    when dis_fields = dat_fields &&
         List.length dis_methods = List.length dat_methods
    ->
      let dis_method_sorted = List.sort (fun (m1,_,_) (m2,_,_) -> String.compare m1 m2) dis_methods in
      let dat_method_sorted = List.sort (fun (m1,_,_) (m2,_,_) -> String.compare m1 m2) dat_methods in
      let compare_method (m1,f1,r1) (m2,f2,r2) =
        m1 = m2 &&
        List.length f1 = List.length f2 &&
        List.equal type_eq f1 f2 &&
        type_eq r1 r2
      in
      List.equal compare_method dis_method_sorted dat_method_sorted
  | _, _ -> false

let tc_expr
  (sclasses : classmapping)
  (vmap : varmapping)
  (expr : typed_info e_expression)
  : (typed_info e_expression * typ) =
    match expr.node with

    (* arithmetic expressions *)
    | ENum n -> (expr, Real)
    | EVar var ->
        begin
        match List.assoc_opt var vmap with
        | Some t -> (expr, t)
        | None -> ({expr with info = { error = Some (VariableNotFound var) } }, Empty)
        end
    | EAdd (lhs, rhs) ->
      begin
        match (List.assoc_opt lhs vmap, List.assoc_opt rhs vmap) with
        | (Some Real, Some Real) -> (expr, Real)
        (* found but not correct type so type error Shape *)
        | (Some _, Some _) -> ({expr with info = {error = Some IllTyped}}, Empty)
        | (None, _) -> ({expr with info = {error = Some (VariableNotFound lhs)}}, Empty)
        | (_, None) -> ({expr with info = {error = Some (VariableNotFound rhs)}}, Empty)
      end
    | EDiv (lhs, rhs) ->
      begin
        match (List.assoc_opt lhs vmap, List.assoc_opt rhs vmap) with
        | (Some Real, Some Real) -> (expr, Real)
        (* found but not correct type so type error Shape *)
        | (Some _, Some _) -> ({expr with info = {error = Some IllTyped}}, Empty)
        | (None, _) -> ({expr with info = {error = Some (VariableNotFound lhs)}}, Empty)
        | (_, None) -> ({expr with info = {error = Some (VariableNotFound rhs)}}, Empty)
      end
    | EEq (lhs, rhs) ->
        begin
        match (List.assoc_opt lhs vmap, List.assoc_opt rhs vmap) with
        | (Some _, Some _) -> (expr, Real)
        | (None, _) -> ({expr with info = {error = Some (VariableNotFound lhs)}}, Empty)
        | (_, None) -> ({expr with info = {error = Some (VariableNotFound rhs)}}, Empty)
        end

    (* object-oriented expressions *)
    | ENewClass (classname, fields) ->
      begin
        match List.assoc_opt classname sclasses with
        | Some (Some ((sfields_t, smethods_t) as s))
          when List.length sfields_t = List.length fields ->
          if List.for_all2 (fun (sfieldname, sfield_t) var ->
              begin
                match List.assoc_opt var vmap with
                | Some var_t when var_t = sfield_t -> true
                | _ -> false
              end)
              sfields_t fields
          then (expr, Shape s)
          else ({expr with info = {error = Some IllTyped}}, Empty)
        (* wrong parameter *)
        | Some _ -> ({expr with info = {error = Some IllTyped}}, Empty)
        | _ -> ({expr with info = {error = Some (VariableNotFound classname)}}, Empty)
      end
    | EFieldAccess (o, fieldname) ->
      begin
        match List.assoc_opt o vmap with
        | Some (Shape (sfields_t, smethods_t)) ->
          begin
            match List.assoc_opt fieldname sfields_t with
            | Some t -> (expr, t)
            | _ -> (* not sure if this is undiclared variable *)
              ({expr with info = {error = Some IllTyped}}, Empty)
          end
        | Some _ -> ({expr with info = {error = Some IllTyped}}, Empty)
        | _ -> ({expr with info = {error = Some (VariableNotFound o)}}, Empty)
      end
    | EMethodCall (o, methodname, params) ->
      begin
        match List.assoc_opt o vmap with
        | Some (Shape (sfield_t, smethods_t)) ->
          begin
            match List.find_opt (fun (smethodname, sparams_t, sreturn_t) -> smethodname = methodname)
              smethods_t with
            | Some (smethodname, sparams_t, sreturn_t)
              when List.length sparams_t = List.length params ->
              let params_match = List.for_all2 (fun sparam_t param ->
                  match List.assoc_opt param vmap with
                  | Some t when type_eq t sparam_t -> true
                  | _ -> false
                )
                sparams_t params
              in
              if params_match
              then (expr, sreturn_t)
              else ({expr with info = {error = Some IllTyped}}, Empty)
            | Some _ -> ({expr with info = {error = Some IllTyped}}, Empty)
            | _ -> (* method does not exist *)
              ({expr with info = {error = Some IllTyped}}, Empty)
          end
        (* found variable but not the type we expect *)
        | Some _ -> ({expr with info = {error = Some IllTyped}}, Empty)
        | _ -> ({expr with info = {error = Some (VariableNotFound o)}}, Empty)
      end
    | EIsA (var, classname) ->
      begin
        match (List.assoc_opt classname sclasses, List.assoc_opt var vmap) with
        | (Some (Some _), Some (Shape _ | Empty)) -> (expr, Real)
        | (Some _, Some _) -> ({expr with info = {error = Some IllTyped}}, Empty)
        | (None, Some _) -> ({expr with info = {error = Some (VariableNotFound classname)}}, Empty)
        | _ -> ({expr with info = {error = Some (VariableNotFound var)}}, Empty)
      end

let tc_def
  (sclasses : classmapping)
  (vmap : varmapping)
  (def : typed_info e_declaration)
  : (typed_info e_declaration * varmapping) =
  let { var; value; } = def.node in
  let (new_expr, t) = tc_expr sclasses vmap value in
  { def with node = {var; value = new_expr}; }, (var, t) :: vmap

let tc_defs
  (sclasses : classmapping)
  (vmap : varmapping)
  (defs : typed_info e_declaration list)
  : (typed_info e_declaration list * varmapping) =
    let (updated_defs, updated_vmap) =
      List.fold_left (fun (updated_defs, updated_vmap) def ->
        let (d, next_vmap) = tc_def sclasses updated_vmap def in
        (d :: updated_defs, next_vmap)
      ) ([], vmap) defs
    in
    (List.rev updated_defs, updated_vmap)

(* Collect assignments which may affect an outer binding. Locals introduced in
   a block shadow that binding and must not invalidate it. *)
let rec assigned_stmt stmt =
  match stmt.node with
  | EAssign (v, _) -> [v]
  | EFieldAssign _ -> []
  | EIf0 (_, a, b) -> assigned_block a @ assigned_block b
  | EWhile0 (_, b) -> assigned_block b
and assigned_block block =
  match block.node with
  | ESimple s -> assigned_stmt s
  | EComplex (defs, stmts) ->
      let locals = List.map (fun d -> d.node.var) defs in
      List.filter (fun v -> not (List.mem v locals))
        (List.concat_map assigned_stmt stmts)

(* After divergent control flow, do not assume an object retains its old
   shape. A later isa cannot safely refine this unknown type either. *)
let invalidate_assigned (vmap : varmapping) names : varmapping =
  List.map (fun (v, t) ->
    if List.mem v names then
      match t with Shape _ -> (v, Ast.Empty) | _ -> (v, t)
    else (v, t)) vmap

let rec tc_stmt
  ?else_vmap
  (sclasses : classmapping)
  (vmap : varmapping)
  (stmt : typed_info e_statement)
  : (typed_info e_statement) =
    (* stmt *)
    match stmt.node with
    | EAssign (var, expr) ->
      let vmap_t_opt = List.assoc_opt var vmap in
      let (new_expr, t) = tc_expr sclasses vmap expr in
      begin
        match vmap_t_opt with
        (* we need them to have same type *)
        | Some vmap_t when type_eq vmap_t t ||
            (match vmap_t, t with Shape _, Shape _ | Empty, Shape _ -> true | _ -> false) ->
          {stmt with node = EAssign (var, new_expr)}
        | Some _ ->
          { node = EAssign (var, new_expr);
            info = { error = Some IllTyped } }
        (* cannot find the variable *)
        | _ ->
          { node = EAssign (var, new_expr);
            info = { error = Some (VariableNotFound var) } }
      end
    | EFieldAssign (o, fieldname, expr) ->
      let (new_expr, t) = tc_expr sclasses vmap expr in
      let vmap_s_opt = List.assoc_opt o vmap in
      begin
        match vmap_s_opt with
        | Some (Shape (fields_t, methods_t)) ->
          begin
            match List.assoc_opt fieldname fields_t with
            | Some vmap_field_t when vmap_field_t = t ->
              {stmt with node = EFieldAssign (o, fieldname, new_expr)}
            | _-> (* not sure whether non-existant field name count as undeclared variable *)
               { node = EFieldAssign (o, fieldname, new_expr);
                 info = { error = Some IllTyped } }
            end
        | _ -> { node = EFieldAssign (o, fieldname, new_expr);
                 info = { error = Some (VariableNotFound o) } }
      end
    | EIf0 (expr, thenblock, elseblock) ->
      let (new_expr, t) = tc_expr sclasses vmap expr in
      (* A successful nominal isa test establishes the tested class's shape in
         the zero branch only. The other branch retains the original shape. *)
      let then_vmap = match expr.node with
        | EIsA (var, classname) ->
          begin match List.assoc_opt var vmap, List.assoc_opt classname sclasses with
          | Some (Shape _ | Empty), Some (Some shape) -> (var, Shape shape) :: vmap
          | _ -> vmap
          end
        | _ -> vmap
      in
      let new_thenblock =
        begin
          match thenblock.node with
          | ESimple thn_stmt ->
            {thenblock with node = ESimple (tc_stmt sclasses then_vmap thn_stmt)}
          | EComplex (decls, stmts) ->
            let (new_defs, new_stmts, _, _) = compute_type sclasses then_vmap decls stmts None in
            {thenblock with node = EComplex (new_defs, new_stmts)}
        end
      in
      let else_vmap = Option.value else_vmap ~default:vmap in
      let new_elseblock =
        begin
          match elseblock.node with
          | ESimple else_stmt ->
            {elseblock with node = ESimple (tc_stmt sclasses else_vmap else_stmt)}
          | EComplex (decls, stmts) ->
            let (new_defs, new_stmts, _, _) = compute_type sclasses else_vmap decls stmts None in
            {elseblock with node = EComplex (new_defs, new_stmts)}
        end
      in
      {stmt with node = EIf0 (new_expr, new_thenblock, new_elseblock)}
    | EWhile0 (expr, block) ->
      let (new_expr, t) = tc_expr sclasses vmap expr in
      let new_block =
        begin
          match block.node with
          | ESimple stmt ->
            {block with node = ESimple (tc_stmt sclasses vmap stmt)}
          | EComplex (decls, stmts) ->
            let (new_defs, new_stmts, _, _) = compute_type sclasses vmap decls stmts None in
            {block with node = EComplex (new_defs, new_stmts)}
        end
      in
      {stmt with node = EWhile0 (new_expr, new_block)}


(* use for both body and block, with expr is None in block *)
and compute_type
  (sclasses : classmapping)
  (vmap : varmapping)
  (decls : typed_info e_declaration list)
  (stmts : typed_info e_statement list)
  (expr  : typed_info e_expression option)
  : (typed_info e_declaration list * typed_info e_statement list * typed_info e_expression option * typ) =
    let (new_defs, new_vmap) = tc_defs sclasses vmap decls in
    (* Direct assignments update the known shape for subsequent statements.
       Joins and loops conservatively forget shapes they may overwrite. *)
    let (rev_stmts, final_vmap, _) = List.fold_left (fun (acc, env, prior) stmt ->
      (* In the false branch of a test immediately following replacement by
         that class, retain the pre-replacement shape as a conservative fallback. *)
      let fallback = match stmt.node with
        | EIf0 ({node = EIsA (v, c); _}, _, _) ->
          begin match List.assoc_opt v prior with
          | Some (previous, Some assigned_class) when assigned_class = c ->
            (v, previous) :: env
          | _ -> env
          end
        | _ -> env
      in
      let checked = tc_stmt ~else_vmap:fallback sclasses env stmt in
      let next_env, next_prior = match stmt.node with
        | EAssign (v, rhs) ->
          let (_, rhs_t) = tc_expr sclasses env rhs in
          begin match List.assoc_opt v env, rhs_t with
          | Some ((Shape _ | Empty) as old), Shape _ ->
            let assigned_class = match rhs.node with ENewClass (c, _) -> Some c | _ -> None in
            (v, rhs_t) :: env, (v, (old, assigned_class)) :: List.remove_assoc v prior
          | _ -> env, List.remove_assoc v prior
          end
        | EIf0 _ | EWhile0 _ -> invalidate_assigned env (assigned_stmt stmt), []
        | EFieldAssign _ -> env, prior
      in
      (checked :: acc, next_env, next_prior)) ([], new_vmap, []) stmts in
    let new_stmts = List.rev rev_stmts in
    let (new_expr, t) = match expr with
    | Some e ->
        let (new_e, t) = tc_expr sclasses final_vmap e in
        (Some new_e, t)
    | None -> (None, Empty)
    in
    (new_defs, new_stmts, new_expr, t)

let tc_method
  (sclasses : classmapping)
  (vmap : varmapping)
  ((_, params_t, ret_t) : methodtyp)
  (meth : typed_info e_classmethod)
  : (typed_info e_classmethod) =
    let { name; params; defs; stmts; body; } = meth.node in
    if List.length params != List.length params_t
    then { meth with info = { error = Some IllTyped } }
    else

    let newvars = List.combine params params_t in
    match compute_type sclasses (newvars @ vmap) defs stmts (Some body) with
    | (new_defs, new_stmts, Some new_expr, t) when type_eq t ret_t ->
        { meth with
          node = {
            name; params; defs = new_defs; stmts = new_stmts; body = new_expr;
          };
        }
    | (new_defs, new_stmts, new_expr, _) ->
        let e = match new_expr with Some e -> e | None -> body in
        { node = {
            name; params; defs = new_defs; stmts = new_stmts; body = e;
          };
          info = {
            error = Some MethodReturnTypeWrong
          };
        }

let tc_class (sclasses : classmapping) (cls : typed_info e_classdef) =
  let shape_opt = List.assoc_opt (get_class_name cls) sclasses in
  match (shape_opt, cls.node) with
  | (Some (Some ((fields_t, methods_t) as s)), { name; fields; methods }) ->
    (* verify if shape names are in order *)
    let shape_field_names = List.map fst fields_t in
    let shape_method_names = List.map (fun (name, _, _) -> name) methods_t in
    let actual_method_names = List.map get_method_name methods in

    (* Printf.eprintf "CLASS %s\n" name;
    List.iter (prerr_endline) fields;
    Printf.eprintf "===\n";
    List.iter (prerr_endline) shape_field_names;
    Printf.eprintf "-----\n";
    List.iter (prerr_endline) shape_method_names;
    Printf.eprintf "===\n";
    List.iter (prerr_endline) actual_method_names;
    Printf.eprintf "CLASS %s\n" name; *)

    let fields_inorder = fields = shape_field_names in
    let methods_match_shape =
      let s1 = List.sort String.compare actual_method_names in
      let s2 = List.sort String.compare shape_method_names in
      List.equal String.equal s1 s2
    in
    let field_duplicated = has_duplicated fields in
    let method_duplicated = has_duplicated actual_method_names in

    (* "fields of a shape do not match" no mentioning about methods
       but we should make sure that the list of methods matches
     *)
    let class_matches_shape = fields_inorder (* && methods_match_shape *) in
    let class_malformed = field_duplicated || method_duplicated in

    if not class_matches_shape || class_malformed
    then { cls with info = { error = Some ClassMalformed } }
    else if not methods_match_shape
    then { cls with info = { error = Some IllTyped } }
    else
    let new_methods = List.map2
      (fun method_t meth -> tc_method sclasses [("this", Shape s)] method_t meth)
      methods_t methods
    in
    {cls with node = { name; fields; methods = new_methods; } }

  | (Some (None), _) -> utc_class sclasses cls
  | _ -> { cls with info = { error = Some IllTyped } }

let check_imports
    (need_types : bool)
    (imports : typed_info e_import list)
    (modules : typed_info e_module list)
    : bool
    =
  (* 1. Lookup helper for module shapes *)
  let get_mod_shape target_name =
    List.find_map (fun m ->
      if m.node.name = target_name then Some m.node.shape else None
    ) modules
  in

  (* 2. Check individual import validity *)
  let is_valid_import imp_ic =
    match imp_ic.node with
    | Import name ->
        if not need_types
        then true
        else
          begin
          (* Must be a typed module (shape is Some (Some _)) *)
          match get_mod_shape name with
          | Some (Some _) -> true
          | _ -> false
          end

    | TypedImport (name, shape) ->
        if not need_types
        then false
        else
          (* If target is typed, shapes must be unifiable/equal
             If the module imported is untyped, ignore
           *)
          match get_mod_shape name with
          | Some None -> true
          (* | Some (Some s) -> false *)
          | _ -> false
  in

  (* 3. Check consistency across all imports of the same name *)
  let rec check_consistency = function
    | [] -> true
    | {node = TypedImport (n1, s1); _} :: rest ->
        List.for_all (fun i ->
          match i.node with
          | TypedImport (n2, s2) when n1 = n2 -> type_eq (Shape s1) (Shape s2)
          | _ -> true
        ) rest && check_consistency rest
    | _ :: rest -> check_consistency rest
  in

  List.for_all is_valid_import imports && check_consistency imports

(* we actually can check bad imports here but for simplicity, we don't *)
let tc_import
  (modules : typed_info e_module list)
  (sclasses : classmapping)
  (import : typed_info e_import)
  : (classmapping * typed_info e_import) =
    let i = match import.node with
      | Import n -> n
      | TypedImport (n, _) -> n
    in
    match List.find_opt (fun m -> m.node.name = i) modules with
    | Some m ->
        let classname = get_class_name m.node.cls in
        let new_mapping = match import.node with
          (* if untyped module, then None; else the module shape *)
          | Import _ -> (classname, m.node.shape) :: sclasses
          (* use this shape *)
          | TypedImport (_, shape) -> (classname, Some shape) :: sclasses
        in
        (new_mapping, import)
    | None ->
        (sclasses, { import with info = { error = Some (UnknownModule i) } })

let tc_imports
  (modules : typed_info e_module list)
  (imports : typed_info e_import list)
  : (classmapping * typed_info e_import list) =
    let (sclasses, type_checked_imports) =
      List.fold_left (fun (sclasses, checked_imports) i ->
        let (updated_sclasses, checked_i) = tc_import modules sclasses i in
        (updated_sclasses, checked_i :: checked_imports)
      ) ([], []) imports
    in
    (sclasses, List.rev type_checked_imports)

let tc_module (modules : typed_info e_module list) (m : typed_info e_module) : typed_info e_module =
  let { node = { name; imports; cls; shape; }; info } = m in
  let has_clean_imports = match shape with
    | Some _ -> check_imports true imports modules
    | _ -> check_imports false imports modules
  in
  let (sclasses, new_imports) = tc_imports modules imports in
  let new_class = tc_class ((get_class_name cls, shape) :: sclasses) cls in
  { node = { m.node with imports = new_imports; cls = new_class; };
    info = { error = if has_clean_imports then None else (Some ImportMixing); }
  }

let tc_modules (modules : typed_info e_module list) : typed_info e_module list =
  let (new_modules, _) = List.fold_left (fun (acc_list, context) m ->
    let updated_m = tc_module context m in
    (updated_m :: acc_list, updated_m :: context)
  ) ([], []) modules in
  List.rev new_modules

let tc_system (s: typed_info e_system) =
  let { node = { modules; imports; decls; stmts; final; }; info } = s in
  let module_duplicate = has_duplicated (List.map (fun m -> m.node.name) modules) in
  let new_modules = tc_modules modules in
  let has_clean_imports = check_imports true imports new_modules in
  let (sclasses, new_imports) = tc_imports modules imports in

  let error =
    if module_duplicate then Some DuplicateModule
    else if not has_clean_imports then Some ImportMixing
    else None
  in

  match compute_type sclasses [] decls stmts (Some final) with
  | (new_decls, new_stmts, Some new_final, Real) ->
      {
        node = {
          modules = new_modules;
          imports = new_imports;
          decls = new_decls;
          stmts = new_stmts;
          final = new_final;
        };
        info = {
          error = error
        };
      }
  | (new_decls, new_stmts, new_final, _) ->
      {
        node = {
          modules = new_modules;
          imports = new_imports;
          decls = new_decls;
          stmts = new_stmts;
          final = match new_final with Some x -> x | None -> final;
        };
        info = {
          error = match error with Some _ -> error | None -> Some IllTyped
        };
      }

let type_check (east : typed_info e_system) =
  let type_checked_east = tc_system east in
  type_checked_east
