type builtin = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Gt | Leq | Geq |
               And | Or | Not | Lst | Cons | Car | Cdr | Append | Empty | If

type var = Int | Double | Bool | String | List of typ | Lambda of typ list * typ
and member = MemConst of string * typ * expr | MemVar of string * typ
and cls = string * member list * string list
and typ = Var of var| Class of cls | Void
and defvar = typ * string * expr
and defmember = member
and defconstructor = string list
and defclass = string * defmember list * defconstructor
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
