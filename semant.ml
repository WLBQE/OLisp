open Ast
open Sast

let check toplevels =
  let rec check_call lamb args = match lamb with
      (VarType(Lambda(_, _)), _) -> raise (Failure "To be implemented: lambda expressions")
    | (BuiltIn, SBuiltIn(builtin)) -> (match builtin with
        Print -> (Void, SCall((BuiltIn, SBuiltIn(Print)), (match args with
          [(VarType(String), exp')] -> [(VarType(String), exp')]
        | [(VarType(Int), exp')] -> [(VarType(Int), exp')]
        | [(VarType(Double), exp')] -> [(VarType(Double), exp')]
        | [(VarType(Bool), exp')] -> [(VarType(Bool), exp')]
        | _ -> raise (Failure "To be implemented: print"))
        ))
      | _ -> raise (Failure "To be implemented: built-in lambda"))
    | _ -> raise (Failure "Invalid call: not a lambda")
  and check_expr = function
    Lit(l) -> (VarType(Int), SLit(l))
  | DoubleLit(l) -> (VarType(Double), SDoubleLit(l))
  | BoolLit(l) -> (VarType(Bool), SBoolLit(l))
  | StringLit(l) -> (VarType(String), SStringLit(l))
  | BuiltIn(builtin) -> (BuiltIn, SBuiltIn(builtin))
  | Id(name) -> raise (Failure "To be implemented: identifiers")
  | MemId(names, name) -> raise (Failure "To be implemented: combined identifiers")
  | Call(lamb, args) -> check_call (check_expr lamb) (List.map check_expr args)
  | Lst(typ, exprs) -> raise (Failure "To be implemented: list constructors")
  | LambdaExpr(typs, ret, formals, exprs) -> raise (Failure "To be implemented: lambda expressions")
  in
  let check_toplevel = function
      Bind(typ, name, expr) -> raise (Failure "To be implemented: bindings")
    | DeclClass(name, memlist, constructorlist) -> raise (Failure "To be implemented: class declarations")
    | Expr(expr) -> SExpr(check_expr expr)
  in List.map check_toplevel toplevels
