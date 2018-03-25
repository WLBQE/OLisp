(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx = 
  | SLit of int
  | SDoubleLit of string
  | SBoolLit of bool
  | SStringLit of string
  | SBuiltIn of built_in
  | SId of string
  | SMemId of string * string
  | SCall of sexpr * sexpr list
  | SLambdaExpr of typ list * ret_typ * string list * sexpr
  | SDefVar of typ * string * sexpr
  | SDefClass of string * smember list * string list
(*   | SExpr of sexpr *)
and smember =
 	SMemConst of string * typ * sexpr
  | SMemVar of string * typ

type sprogram = sexpr list

let rec string_of_smember = function
	SMemConst(name, typ, sexpr) -> "(smember (" ^ string_of_typ typ ^ " " ^ name ^ ")" ^ string_of_sexpr sexpr ^ ")"
  | SMemVar(name, typ) -> "(smember (" ^ string_of_typ typ ^ " " ^ name ^ "))"

 and string_of_smember_list = function
 	[] -> ""
  | [smember] -> string_of_smember smember
  | smember :: tl -> string_of_smember smember ^ " " ^ string_of_smember_list tl

and string_of_sexpr(t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
  	SLit(lit) -> string_of_int lit
  | SDoubleLit(dlit) -> dlit
  | SBoolLit(blit) -> string_of_bool blit
  | SStringLit(slit) -> "\"" ^ String.escaped slit ^ "\""
  | SBuiltIn(builtin) -> string_of_built_in builtin
  | SId(id) -> id
  | SMemId(cls, mem) -> cls ^ "." ^ mem
  | SCall(exp, exps) -> "(" ^ string_of_sexpr exp ^ " " ^ string_of_sexpr_list exps ^ ")"
  | SLambdaExpr(typ_list, ret_typ, formal_list, expr) ->
      "(lambda (" ^ string_of_typ_list typ_list ^ " -> " ^ string_of_ret_typ ret_typ ^ ") ("
        ^ string_of_formal_list formal_list ^ ") " ^ string_of_sexpr expr ^ ")"
  | SDefVar(typ, name, expr) -> "(define (" ^ string_of_typ typ ^ " " ^ name ^ ") " ^ string_of_sexpr expr ^ ")"
  | SDefClass(name, members, formals) ->
      "(class " ^ name ^ " " ^ string_of_smember_list members ^ "(constructor " ^ string_of_formal_list formals ^ "))"
  ) ^ ")"

and string_of_sexpr_list = function
	[] -> ""
  | [sexpr] -> string_of_sexpr sexpr
  | sexpr :: tl -> string_of_sexpr sexpr ^ " " ^ string_of_sexpr_list tl

let string_of_sprogram = string_of_sexpr_list program
