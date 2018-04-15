open Ast
open Sast

module StringMap = Map.Make(String)

let check toplevels =
  let confirm_type typ (ret, _) =
    if ret = VarType(typ) then typ else raise (Failure "error: types do not match")
  in
  let rec check_call lamb args = ((match lamb with
      (VarType(Lambda(types, ret)), _) -> ignore(List.map2 confirm_type types args); ret
    | (BuiltIn(builtin), _) -> let check_same_type exprs = match exprs with
        [] -> []
      | expr :: _ -> let match_ret_type (r1, _) (r2, e2) =
        if r1 = r2 then (r2, e2) else raise (Failure "error: expressions of the same type expected") in
        List.map (match_ret_type expr) exprs
      in
      (match builtin with
        Add | Mult -> (match check_same_type args with
          (VarType(Int), _) :: _ when List.length args > 1 -> VarType(Int)
        | (VarType(Double), _) :: _ when List.length args > 1 -> VarType(Double)
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | Sub | Div -> (match args with
          [(VarType(Int), _); (VarType(Int), _)] -> VarType(Int)
        | [(VarType(Double), _); (VarType(Double), _)] -> VarType(Double)
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | Mod -> (match args with
          [(VarType(Int), _); (VarType(Int), _)] -> VarType(Int)
        | _ -> raise (Failure "mod: invalid arguments"))
      | Eq | Neq -> (match args with
          [(VarType(Int), _); (VarType(Int), _)] | [(VarType(Double), _); (VarType(Double), _)]
        | [(VarType(Bool), _); (VarType(Bool), _)] | [(VarType(String), _); (VarType(String), _)] -> VarType(Bool)
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | Lt | Gt | Leq | Geq -> (match args with
          [(VarType(Int), _); (VarType(Int), _)] | [(VarType(Double), _); (VarType(Double), _)] -> VarType(Bool)
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | And | Or -> (match check_same_type args with
          (VarType(Bool), _) :: _ when List.length args > 1 -> VarType(Bool)
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | Not -> (match args with
          [(VarType(Bool), _)] -> VarType(Bool)
        | _ -> raise (Failure "not: invalid argument"))
      | I2d -> (match args with
          [(VarType(Int), _)] -> VarType(Double)
        | _ -> raise (Failure "i2d: invalid argument"))
      | D2i -> (match args with
          [(VarType(Double), _)] -> VarType(Int)
        | _ -> raise (Failure "d2i: invalid argument"))
      | Cons -> (match args with
          [(VarType(t1), _); (VarType(List(t2)), _)] when t1 = t2 -> VarType(List(t1))
        | _ -> raise (Failure "cons: invalid arguments"))
      | Car -> (match args with
          [(VarType(List(t)), _)] -> VarType(t)
        | _ -> raise (Failure "car: invalid argument"))
      | Cdr -> (match args with
          [(VarType(List(t)), _)] -> VarType(List(t))
        | _ -> raise (Failure "cdr: invalid argument"))
      | Append -> (match args with
          [(VarType(List(t1)), _); (VarType(List(t2)), _)] when t1 = t2 -> VarType(List(t1))
        | _ -> raise (Failure "append: invalid arguments"))
      | Empty -> (match args with
          [(VarType(List(_)), _)] -> VarType(Bool)
        | _ -> raise (Failure "empty: invalid argument"))
      | If -> (match args with
          [(VarType(Bool), _); (t1, _); (t2, _)] when t1 = t2 -> t1
        | _ -> raise (Failure "if: invalid arguments"))
      | Begin -> (match List.rev args with
          (t1, _) :: _ -> t1
        | _ -> raise (Failure "begin: expressions expected"))
      | Print -> (match args with
          [(VarType(String), _)] | [(VarType(Int), _)] | [(VarType(Double), _)] | [(VarType(Bool), _)] -> Void
        | _ -> raise (Failure "print: invalid argument")))
    | _ -> raise (Failure "invalid call: not a lambda")), SCall(lamb, args))
  and check_expr syms cls = function
    Lit(l) -> (VarType(Int), SLit(l))
  | DoubleLit(l) -> (VarType(Double), SDoubleLit(l))
  | BoolLit(l) -> (VarType(Bool), SBoolLit(l))
  | StringLit(l) -> (VarType(String), SStringLit(l))
  | BuiltIn(builtin) -> (BuiltIn(builtin), SBuiltIn(builtin))
  | Id(name) -> let rec type_of_id id = function
      [] -> raise (Failure ("undeclared identifier: " ^ id))
    | sym :: rest -> try StringMap.find id sym with Not_found -> type_of_id id rest
    in
    (VarType(type_of_id name syms), SId(name))
  | MemId(names, name) -> raise (Failure "to be implemented: combined identifiers")
  | Call(lamb, args) -> check_call (check_expr syms cls lamb) (List.map (check_expr syms cls) args)
  | Lst(typ, exprs) -> let exprs' = List.map (check_expr syms cls) exprs in
      ignore (List.map (confirm_type typ) exprs'); (VarType(List(typ)), SLst(typ, exprs'))
  | LambdaExpr(typs, ret, formals, expr) -> raise (Failure "to be implemented: lambda expressions")
  in
  let check_toplevel (sym, cls, checked) = function
      Bind(typ, name, expr) -> if StringMap.mem name cls || StringMap.mem name sym
      then raise (Failure ("name " ^ name ^ " is already used"))
      else (match typ with
          Lambda(_, _) -> let sym' = StringMap.add name typ sym in (sym', cls,
            let expr' = check_expr [sym'] cls expr in SBind(confirm_type typ expr', name, expr') :: checked)
        | _ -> (StringMap.add name typ sym, cls,
          let expr' = check_expr [sym] cls expr in SBind(confirm_type typ expr', name, expr') :: checked))
    | DeclClass(name, memlist, constructorlist) -> raise (Failure "to be implemented: class declarations")
    | Expr(expr) -> (sym, cls, SExpr(check_expr [sym] cls expr) :: checked)
  in
  let (sym, cls, checked) = List.fold_left check_toplevel (StringMap.empty, StringMap.empty, []) toplevels in
  (sym, cls, List.rev checked)
