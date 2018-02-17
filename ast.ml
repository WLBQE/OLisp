type builtin = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Gt | Leq | Geq |
               And | Or | Not | Lst | Cons | Car | Cdr | Append | Empty | If

type basic = Int | Double | Bool | String | List of typ
and lambda = typ list * typ
and member = MemConst of string * typ * expr | MemVar of typ * string
and cls = string * member list * string list
and typ = Basic of basic | Lambda of lambda | Class of cls | Void
and defvar = typ * string * expr
and deflambda = lambda * string * string list * expr
and defmember = member
and defconstructor = string list
and defclass = string * defmember list * defconstructor
and def = DefVar of defvar | DefLambda of deflambda | DefClass of defclass
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
