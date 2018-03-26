open Ast
open Sast

module StringMap = Map.Make(String)

let check toplevels =
  let check_built_in = function
      Print -> (Void, SBuiltIn(Print))
    | _ -> raise (Failure "To be implemented: built-in lambda")
  in
  let check_call lamb args = match lamb with
      (VarType(Lambda), _) -> raise (Failure "To be implemented: lambda expressions")
    | _ -> raise (Failure "To be implemented")
  in
  let check_expr = function
    Lit(l) -> (VarType(Int), SLit(l))
  | DoubleLit(l) -> (VarType(Double), SDoubleLit(l))
  | Bool(l) -> (VarType(Bool), SBoolLit(l))
  | StringLit(l) -> (VarType(String), SStringLit(l))
  | BuiltIn(built_in) -> (VarType(BuiltIn), SBuiltIn(built_in))
  | Id(name) -> raise (Failure "To be implemented: identifiers")
  | MemId(names, name) -> raise (Failure "To be implemented: combined identifiers")
  | Call(lamb, args) -> check_call (check_expr lamb) (List.map check_expr args)
  | Lst(typ, exprs) -> raise (Failure "To be implemented: list constructors")
  | LambdaExpr -> raise (Failure "To be implemented: lambda expressions")
  in
  let check_toplevel = function
      Bind(typ, name, expr) -> raise (Failure "To be implemented: bindings")
    | DeclClass(name, memlist, constructorlist) -> raise (Failure "To be implemented: class declarations")
    | Expr(expr) -> check_expr expr
  in List.map check_toplevel toplevels
