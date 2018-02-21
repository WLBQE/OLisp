type builtin =
    Add | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Gt | Leq | Geq
  | And | Or | Not
  | Lst | Cons | Car | Cdr | Append | Empty
  | If | Begin

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
  | BuiltIn of builtin
  | Id of string
  | Call of expr * expr list
  | Lambda of typ list * ret_typ * string list * expr
  | DefVar of typ * string * expr
  | DefClass of string * member list * string list
and member =
    MemConst of string * typ * expr
  | MemVar of string * typ

type program = expr list
