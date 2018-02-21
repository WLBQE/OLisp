type built_in =
    Add | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Gt | Leq | Geq
  | And | Or | Not
  | Lst | Cons | Car | Cdr | Append | Empty
  | If | Begin | Print

type typ =
    Int
  | Double
  | Bool
  | String
  | List of typ
  | Lambda of typ list * ret_typ
  | Class of string
and ret_typ =
    VarType of typ
  | Void

type expr =
    Lit of int
  | DoubleLit of string
  | BoolLit of bool
  | StringLit of string
  | BuiltIn of built_in
  | Id of string
  | Call of expr * expr list
  | LambdaExpr of typ list * ret_typ * string list * expr
  | DefVar of typ * string * expr
  | DefClass of string * member list * string list
and member =
    MemConst of string * typ * expr
  | MemVar of string * typ

type program = expr list

let string_of_built_in = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | Not -> "not"
  | Lst -> "list"
  | Cons -> "cons"
  | Car -> "car"
  | Cdr -> "cdr"
  | Append -> "append"
  | Empty -> "empty"
  | If -> "if"
  | Begin -> "begin"
  | Print -> "print"

let rec string_of_typ = function
    Int -> "int"
  | Double -> "double"
  | Bool -> "bool"
  | String -> "string"
  | List(typ) -> "[" ^ (string_of_typ typ) ^ "]"
  | Lambda(typ_list, ret_typ) -> "(" ^ (string_of_typ_list typ_list) ^ " -> " ^ (string_of_ret_typ ret_typ) ^ ")"
  | Class(name) -> name

and string_of_typ_list = function
    [] -> ""
  | [typ] -> string_of_typ typ
  | typ :: tl -> (string_of_typ typ) ^ " " ^ (string_of_typ_list tl)

and string_of_ret_typ = function
    VarType(typ) -> string_of_typ typ
  | Void -> "void"

let rec string_of_formal_list = function
    [] -> ""
  | [name] -> name
  | name :: tl -> name ^ " " ^ (string_of_formal_list tl)

let rec string_of_member = function
    MemConst(name, typ, expr) -> "(member " ^ (string_of_typ typ) ^ " " ^ name ^ ") " ^ (string_of_expr expr) ^ ")"
  | MemVar(name, typ) -> "(member " ^ (string_of_typ typ) ^ " " ^ name ^ "))"

and string_of_member_list = function
    [] -> ""
  | [member] -> string_of_member member
  | member :: tl -> (string_of_member member) ^ " " ^ (string_of_member_list tl)

and string_of_expr = function
    Lit(lit) -> string_of_int lit
  | DoubleLit(dlit) -> dlit
  | BoolLit(blit) -> string_of_bool blit
  | StringLit(slit) -> "\"" ^ (String.escaped slit) ^ "\""
  | BuiltIn(builtin) -> string_of_built_in builtin
  | Id(id) -> id
  | Call(exp, exps) -> "(" ^ (string_of_expr exp) ^ " " ^ (string_of_expr_list exps) ^ ")"
  | LambdaExpr(typ_list, ret_typ, formal_list, expr) ->
      "(lambda (" ^ (string_of_typ_list typ_list) ^ " -> " ^ (string_of_ret_typ ret_typ) ^ ") ("
        ^ (string_of_formal_list formal_list) ^ ") " ^ (string_of_expr expr) ^ ")"
  | DefVar(typ, name, expr) -> "(define (" ^ (string_of_typ typ) ^ " " ^ name ^ ") " ^ (string_of_expr expr) ^ ")"
  | DefClass(name, members, formals) ->
      "(class " ^ name ^ " " ^ (string_of_member_list members) ^ "(constructor " ^ (string_of_formal_list formals) ^ "))"

and string_of_expr_list = function
    [] -> ""
  | [expr] -> string_of_expr expr
  | expr :: tl -> (string_of_expr expr) ^ " " ^ (string_of_expr_list tl)

let string_of_program program = string_of_expr_list program
