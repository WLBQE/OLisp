type built_in =
    Add | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Gt | Leq | Geq
  | And | Or | Not
  | I2d | D2i
  | Cons | Car | Cdr | Append | Empty
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
  | BuiltInTyp of built_in
  | Void

type expr =
    Lit of int
  | DoubleLit of string
  | BoolLit of bool
  | StringLit of string
  | BuiltIn of built_in
  | Id of string
  | MemId of string * string list * string
  | Call of expr * expr list
  | Lst of typ * expr list
  | LambdaExpr of typ list * ret_typ * string list * expr

type member = string * typ

type toplevel =
    Bind of typ * string * expr
  | DeclClass of string * member list * string list
  | Expr of expr

type program = toplevel list

let split_mem_id mem_id =
  let str_list = Str.split (Str.regexp "\\.") mem_id in
  MemId (List.hd str_list, List.rev (List.tl (List.rev (List.tl str_list))), List.hd (List.rev str_list))

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
  | I2d -> "i2d"
  | D2i -> "d2i"
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
  | List typ -> "[" ^ string_of_typ typ ^ "]"
  | Lambda (typ_list, ret_typ) -> "(" ^ string_of_typ_list typ_list ^ "-> " ^ string_of_ret_typ ret_typ ^ ")"
  | Class name -> name
and string_of_typ_list = function
    [] -> "void "
  | lst -> List.fold_left (fun str typ -> str ^ string_of_typ typ ^ " ") "" lst
and string_of_ret_typ = function
    VarType typ -> string_of_typ typ
  | BuiltInTyp builtin -> "builtin " ^ string_of_built_in(builtin)
  | Void -> "void"

let rec string_of_formal_list = function
    [] -> ""
  | [name] -> name
  | name :: tl -> name ^ " " ^ string_of_formal_list tl

let rec string_of_expr = function
    Lit lit -> string_of_int lit
  | DoubleLit dlit -> dlit
  | BoolLit blit -> string_of_bool blit
  | StringLit slit -> "\"" ^ String.escaped slit ^ "\""
  | BuiltIn builtin -> string_of_built_in builtin
  | Id id -> id
  | MemId (first, middle, last) -> first ^ "." ^ List.fold_left (fun str name -> str ^ name ^ ".") "" middle ^ last
  | Call (exp, exps) ->
    "(" ^ string_of_expr exp ^ List.fold_left (fun str exp -> str ^ " " ^ string_of_expr exp) "" exps ^ ")"
  | Lst (typ, exps) ->
    "(list " ^ string_of_typ typ ^ List.fold_left (fun str exp -> str ^ " " ^ string_of_expr exp) "" exps ^ ")"
  | LambdaExpr (typ_list, ret_typ, formal_list, expr) ->
    "(lambda (" ^ string_of_typ_list typ_list ^ "-> " ^ string_of_ret_typ ret_typ ^ ") ("
      ^ string_of_formal_list formal_list ^ ") " ^ string_of_expr expr ^ ")"

let string_of_member (name, typ) = "(member (" ^ string_of_typ typ ^ " " ^ name ^ "))"

let string_of_top_level = function
    Bind (typ, name, expr) -> "(define (" ^ string_of_typ typ ^ " " ^ name ^ ") " ^ string_of_expr expr ^ ")\n"
  | DeclClass (name, members, formals) ->
    "(class " ^ name ^ " " ^ List.fold_left (fun str mem -> str ^ string_of_member mem ^ " ") "" members
      ^ "(constructor" ^ List.fold_left (fun str formal -> str ^ " " ^ formal) "" formals ^ "))\n"
  | Expr expr -> string_of_expr expr ^ "\n"

let rec string_of_top_level_list = function
    [] -> ""
  | [top_level] -> string_of_top_level top_level
  | top_level :: tl -> string_of_top_level top_level ^ string_of_top_level_list tl

let string_of_program program = string_of_top_level_list program
