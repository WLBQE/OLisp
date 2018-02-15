type builtin = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Gt | Leq | Geq |
               And | Or | Not | Lst | Cons | Car | Cdr | Append | Empty

type var = Int | Double | Bool | String | List of typ | Class of cls
and lambda = typ list * typ
and typ = Var of var | Lambda of lambda
and bind = typ * string
and cls = member list * string list
and member = MemConst of bind | MemVar of bind * expr
and defvar = bind * expr
and deflambda = lambda * string * string list * expr
and def = DefVar of defvar | DefLambda of deflambda
and expr = 
    Lit of int
  | DoubleLit of string
  | BoolLit of bool
  | StringLit of string
  | Builtin of builtin
  | Id of string
  | Begin of expr list
  | Call of expr * expr list
  | AnnoLambda of lambda * string list * expr
  | Define of def
