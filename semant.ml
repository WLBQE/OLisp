open Ast
open Sast

module StringMap = Map.Make(String)

let check toplevels =
  let confirm_type typ (ret, _) =
    if ret = VarType(typ) then typ else raise (Failure "error: types do not match")
  in
  let rec check_call lamb args = ((match lamb with
      (VarType(Lambda(types, ret)), _) -> ignore(List.map2 confirm_type types args); ret
    | (BuiltIn(builtin), _) -> let check_same_type exprs =
        let rec check_same_type_rest typ rest = match rest with
            [] -> []
          | (t1, e1) :: tl when t1 = typ -> check_same_type_rest typ tl
          | _ -> raise (Failure "expected expressions of the same type")
        in
        match exprs with
            [] -> []
          | [(t1, e1)] -> [(t1, e1)]
          | (t1, e1) :: tl -> (t1, e1) :: check_same_type_rest t1 tl
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
  and check_expr = function
    Lit(l) -> (VarType(Int), SLit(l))
  | DoubleLit(l) -> (VarType(Double), SDoubleLit(l))
  | BoolLit(l) -> (VarType(Bool), SBoolLit(l))
  | StringLit(l) -> (VarType(String), SStringLit(l))
  | BuiltIn(builtin) -> (BuiltIn(builtin), SBuiltIn(builtin))
  | Id(name) -> raise (Failure "to be implemented: identifiers")
  | MemId(names, name) -> raise (Failure "to be implemented: combined identifiers")
  | Call(lamb, args) -> check_call (check_expr lamb) (List.map check_expr args)
  | Lst(typ, exprs) -> let exprs' = List.map check_expr exprs in
      ignore (List.map (confirm_type typ) exprs'); (VarType(List(typ)), SLst(typ, exprs'))
  | LambdaExpr(typs, ret, formals, exprs) -> raise (Failure "to be implemented: lambda expressions")
  in
  let check_toplevel (sym, checked) toplevel = match toplevel with
      Bind(typ, name, expr) -> raise (Failure "to be implemented: bindings")
    | DeclClass(name, memlist, constructorlist) -> raise (Failure "to be implemented: class declarations")
    | Expr(expr) -> (sym, SExpr(check_expr expr) :: checked)
  in
  List.fold_left check_toplevel (StringMap.empty, []) toplevels
