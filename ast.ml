type builtin = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Gt | Leq | Geq |
               And | Or | Not | Lst | Cons | Car | Cdr | Append | Empty | If

type var = Int | Double | Bool | String | List of typ | Lambda of typ list * typ | Class
and member = MemConst of string * typ * expr | MemVar of string * typ
and typ = Var of var | Void
and defvar = var * string * expr
and constructor = string list
and defclass = string * member list * constructor
and def = DefVar of defvar | DefClass of defclass
and expr =
    Lit of int
  | DoubleLit of string
  | BoolLit of bool
  | StringLit of string
  | Builtin of builtin
  | Id of string
  | Begin of expr list
  | Call of expr * expr list
  | LambdaExpr of typ list * typ * string list * expr
  | Define of def

type program = expr