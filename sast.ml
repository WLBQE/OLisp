open Ast

type styp =
    SInt
  | SDouble
  | SBool
  | SString
  | SList of styp
  | SLambda of styp list * sret_typ
  | SClass of string
and sret_typ =
    SVarType of styp
  | SBuiltInTyp of built_in
  | SVoid

type sexpr = sret_typ * sx
and sx =
    SLit of int
  | SDoubleLit of string
  | SBoolLit of bool
  | SStringLit of string
  | SBuiltIn of built_in
  | SId of string
  | SMemId of (string * string) * (string * string) list * string
  | SCall of sexpr * sexpr list
  | SLst of styp * sexpr list
  | SLambdaExpr of styp list * sret_typ * string list * sexpr

type smember = string * styp

type stoplevel =
    SBind of styp * string * sexpr
  | SDeclClass of string * smember list * string list
  | SExpr of sexpr

type sprogram = stoplevel list

let rec string_of_styp = function
    SInt -> "int"
  | SDouble -> "double"
  | SBool -> "bool"
  | SString -> "string"
  | SList typ -> "[" ^ string_of_styp typ ^ "]"
  | SLambda (typ_list, ret_typ) -> "(" ^ string_of_styp_list typ_list ^ "-> " ^ string_of_sret_typ ret_typ ^ ")"
  | SClass name -> name
and string_of_styp_list = function
    [] -> "void "
  | lst -> List.fold_left (fun str typ -> str ^ string_of_styp typ ^ " ") "" lst
and string_of_sret_typ = function
    SVarType typ -> string_of_styp typ
  | SBuiltInTyp builtin -> "builtin " ^ string_of_built_in(builtin)
  | SVoid -> "void"

let string_of_sprogram =
  let rec string_of_sexpr (t, e) =
    let exp_str = match e with
        SLit lit -> string_of_int lit
      | SDoubleLit dlit -> dlit
      | SBoolLit blit -> string_of_bool blit
      | SStringLit slit -> "\"" ^ String.escaped slit ^ "\""
      | SBuiltIn builtin -> string_of_built_in builtin
      | SId id -> id
      | SMemId ((first, typ), middle, last) -> "(" ^ typ ^ " : " ^ first ^ ")."
        ^ List.fold_left (fun str (name, typ) -> str ^ "(" ^ typ ^ " : " ^ name ^ ").") "" middle ^ last
      | SCall (exp, exps) ->
        "(" ^ string_of_sexpr exp ^ List.fold_left (fun str exp -> str ^ " " ^ string_of_sexpr exp) "" exps ^ ")"
      | SLst (typ, exps) ->
        "(list " ^ string_of_styp typ ^ List.fold_left (fun str exp -> str ^ " " ^ string_of_sexpr exp) "" exps ^ ")"
      | SLambdaExpr (typ_list, ret_typ, formal_list, expr) ->
        "(lambda (" ^ string_of_styp_list typ_list ^ "-> " ^ string_of_sret_typ ret_typ ^ ") ("
          ^ string_of_formal_list formal_list ^ ") " ^ string_of_sexpr expr ^ ")"
    in
    "(" ^ string_of_sret_typ t ^ " : " ^ exp_str ^ ")"
  in
  let string_of_smember (name, typ) = "(member (" ^ string_of_styp typ ^ " " ^ name ^ "))" in
  let string_of_stop_level = function
      SBind (typ, name, expr) -> "(define (" ^ string_of_styp typ ^ " " ^ name ^ ") " ^ string_of_sexpr expr ^ ")\n"
    | SDeclClass (name, members, formals) ->
      "(class " ^ name ^ " " ^ List.fold_left (fun str mem -> str ^ string_of_smember mem ^ " ") "" members
        ^ "(constructor" ^ List.fold_left (fun str formal -> str ^ " " ^ formal) "" formals ^ "))\n"
    | SExpr expr -> string_of_sexpr expr ^ "\n"
  in
  List.fold_left (fun str top_level -> str ^ string_of_stop_level top_level) ""
