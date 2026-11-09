open Ast
open Mapper

open Effect
open Effect.Deep

type error =
  | UndeclareClass of string
  | UndeclareVariable of string
  | UnhandledStatement of statement
  | UnhandledExpression of expression
  | UnknownModule of string
  | DuplicateModuleName
  | DuplicateClassName
  | DuplicateMethodName
  | DuplicateFieldName
  | DuplicateParameterName
  | ImportMixing
  | TypeError
  | ClassShapeError
  | InternalError
  [@@deriving show, eq]

type _ Effect.t += ValidatorError : error -> 'a Effect.t

let rec has_duplicated names =
  match names with
  | [] -> false
  | hd :: tail ->
      if List.mem hd tail then
        true
      else
        has_duplicated tail

type _ Effect.t +=
  | CheckClassExist : classname -> classname Effect.t
  | CheckVariableDeclared : name -> name Effect.t

module ValidatorConfig = struct
  include DefaultMapper
  let map_classname c = Effect.perform (CheckClassExist c)
  let map_variable v = Effect.perform (CheckVariableDeclared v)

  let enter_scope = function
    | ScopeSystem s -> Effect.perform (EnterSystem s)
    | ScopeModule m -> Effect.perform (EnterModule m)
    | ScopeImport i -> Effect.perform (EnterImport i)
    | ScopeClass c -> Effect.perform (EnterClass c)
    | ScopeMethod m -> Effect.perform (EnterMethod m)
    | ScopeDef d -> Effect.perform (EnterDef d)
    | _ -> ()
end

(* Validation Pass for both System and Program
   Both share similarities, with System needs handling for
   Module and System, therefore, we can compress them into
   one Runner, and let the Effect Handlers handles the state
   for each pass
 *)
module ValidatorRunner = MakeMapper(ValidatorConfig)

type program_validator_state = {
  classes : classname list;
  names   : variable list;
}

let validate = function
  | MixedSystem _ as system ->
      (* typed system [according to assignment] needs a different validation
         we need to record informations, rather than throwing error immedidately
         but we can only link a normal ast, so we return itself

         we turn the ast into an extended ast (eAST) with information
         (called information carrying AST node)

         the information for validator will be a list of errors
         including basic validation errors and type errors
       *)
      let open Typechecker in
      let open Typechecker.MapperConfig in
      let module Lifter = East.MakeLifter(Typechecker.Info) in
      (* delegate all checks to this, by the rules of type checker
         we expect it to also detect all trivial validator errors

         unknown modules are caught by import(s) rules
         duplicated modules are caught by system rules
         unexpected method, fields or parameter names are detected by class rule

         undeclared variables are caught by stmt/expr rules
         unknown field access, method calls are caught by stmt/expr rules
       *)
      let east = type_check (Lifter.lift system) in

      let handle_enter : 'n.
        Emapper.enter_type
        -> ('n, typed_info) East.ic
        -> (typed_info, 's -> 'r) continuation
        -> 's
        -> 'r
        = fun t m k state ->
          let next_state =
            match (t, m.info.error) with
            | (Enter, Some e) -> e :: state
            | _ -> state
          in
          (continue k m.info) next_state
      in
      let errors = match_with TypedValidatorRunner.map_system east
      {
        retc = (fun x state -> state); (* Return the accumulated errors *)
        exnc = raise;

        effc = fun (type e) (eff : e Effect.t) ->
          (match eff with
          | EnterEModule (t, m) -> Some (fun k -> handle_enter t m k)
          | EnterEImport (t, m) -> Some (fun k -> handle_enter t m k)
          | EnterEClass   (t, m) -> Some (fun k -> handle_enter t m k)
          | EnterEMethod   (t, m) -> Some (fun k -> handle_enter t m k)
          | EnterESystem (t, m) -> Some (fun k -> handle_enter t m k)
          | EnterEExpr   (t, m) -> Some (fun k -> handle_enter t m k)
          | EnterEStmt   (t, m) -> Some (fun k -> handle_enter t m k)
          | EnterEBlock   (t, m) -> Some (fun k -> handle_enter t m k)
          | EnterEDef   (t, m) -> Some (fun k -> handle_enter t m k)
          | _ -> None

          : ((e, type_error list -> type_error list) continuation
            -> type_error list
            -> type_error list
          ) option)
      }
      []
      in

      let rank = function
        | DuplicateModule        -> 0
        | UnknownModule _        -> 1
        | ImportMixing           -> 2
        | ClassMalformed         -> 3
        | VariableNotFound _     -> 4
        | _                      -> 5 (* All TypeErrors *)
      in

      let _ = match List.sort (fun a b -> compare (rank a) (rank b)) errors with
        | [] -> ()
        | top_priority :: _ ->
            (match top_priority with
             | DuplicateModule -> Effect.perform (ValidatorError DuplicateModuleName)
             | UnknownModule n -> Effect.perform (ValidatorError (UnknownModule n))
             | ImportMixing -> Effect.perform (ValidatorError ImportMixing)
             | ClassMalformed  -> Effect.perform (ValidatorError ClassShapeError)
             | VariableNotFound v -> Effect.perform (ValidatorError (UndeclareVariable v))
             | _ -> Effect.perform (ValidatorError TypeError))
      in
      system

  | Program (classes, _, _, _) as program ->
    (* Validate Program only *)
    let handler s = match_with ValidatorRunner.main s
    {
      retc = (fun x state -> x);
      exnc = raise;

      effc = fun (type e) (eff : e Effect.t) ->
        (match eff with
        | EnterClass (Class (_, params, methods, _)) -> Some (fun k state ->
            let method_names = List.map (fun (Method (n, _, _, _, _)) -> n) methods in
            let has_duplicated_param = has_duplicated params in
            let has_duplicated_method = has_duplicated methods in
            match (has_duplicated_param, has_duplicated_method) with
            | (true, _) -> Effect.perform (ValidatorError (DuplicateFieldName))
            | (_, true) -> Effect.perform (ValidatorError (DuplicateMethodName))
            | _ -> continue k () state
          )
        | EnterMethod (Method (_, params, _, _, _)) -> Some (fun k state ->
            (* reset the name list on method call *)
            let new_state = { state with names = "this" :: params } in
            let has_duplicated_param = has_duplicated params in
            if has_duplicated_param
            then Effect.perform (ValidatorError (DuplicateParameterName))
            else continue k () new_state
          )
        | EnterDef (Def (n, _)) -> Some (fun k state ->
            continue k () { state with names = n :: state.names }
          )
        | EnterSystem (Program (_, defs, _, _)) -> Some (fun k state ->
            (* reset the name list *)
            let new_state = { state with names = []; } in
            continue k () new_state
          )
        | CheckClassExist (classname) -> Some (fun k state ->
            if List.mem classname state.classes
            then continue k classname state
            else Effect.perform (ValidatorError (UndeclareClass classname))
          )
        | CheckVariableDeclared (var) -> Some (fun k state ->
            if List.mem var state.names
            then continue k var state
            else Effect.perform (ValidatorError (UndeclareVariable var))
          )
        | EnterModule _ -> Some (fun k state -> continue k () state)
        | EnterImport _ -> Some (fun k state -> continue k () state)
        | _ -> None
        : ((e, program_validator_state -> Ast.t) continuation -> program_validator_state -> Ast.t) option)
    }
    in
    let classnames = List.map (fun (Class (n, _, _, _)) -> n) classes in
    if has_duplicated classnames
    then Effect.perform (ValidatorError (DuplicateClassName))
    else handler program { classes = classnames; names = []; }

let run (ast : Ast.t) : Ast.t =
  match ast with
  | MixedSystem s as system -> validate system
  | Program p as program -> validate program
