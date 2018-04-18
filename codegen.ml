module L = Llvm
open Sast

module StringMap = Map.Make(String)

let translate sprogram =
  let context  = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context in
  let double_t = L.double_type context in
  let void_t = L.void_type context in
  let the_module = L.create_module context "OLisp" in

  let rec ltype_of_styp = function
      SInt -> i32_t
    | SDouble -> double_t
    | SBool -> i1_t
    | SString -> L.pointer_type i8_t
    | _ -> raise (Failure "to be implemented")
  in
  let rec ltype_of_sret_typ = function
      SVarType typ -> ltype_of_styp typ
    | SBuiltInTyp builtin -> raise (Failure "to be implemented")
    | SVoid -> void_t
  in

  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let build_program (sym, cls, stoplevels) =
    let main_ty = L.function_type i32_t [||] in
    let main_function = L.define_function "main" main_ty the_module in
    let main_builder = L.builder_at_end context (L.entry_block main_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "i_fmt" main_builder in
    let double_format_str = L.build_global_stringptr "%g\n" "f_fmt" main_builder in
    let string_format_str = L.build_global_stringptr "%s\n" "s_fmt" main_builder in
    let true_str = L.build_global_stringptr "true" "true" main_builder in
    let false_str = L.build_global_stringptr "false" "false" main_builder in
    let rec build_expr builder (_, e) = (match e with
        SLit i -> L.const_int i32_t i
      | SDoubleLit d -> L.const_float_of_string double_t d
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SStringLit s -> L.build_global_stringptr s "str" main_builder
      | SCall (lamb, exprs) -> (match lamb with
          (SBuiltInTyp builtin, _) -> (match builtin with
            Print -> L.build_call printf_func (let e = List.hd exprs in match e with
                (SVarType SInt, _) -> [|int_format_str; build_expr builder e|]
              | (SVarType SDouble, _) -> [|double_format_str; build_expr builder e|]
              | (SVarType SBool, _) -> [|string_format_str;
                  if build_expr builder e = L.const_int i1_t 1 then true_str else false_str|]
              | (SVarType SString, _) -> [|string_format_str; build_expr builder e|]
              | _ -> raise (Failure "compiler bug"))
            "printf" builder
          | _ -> raise (Failure "to be implemented: built-in"))
        | (SVarType (SLambda _), _) -> raise (Failure "to be implemented: lambda expressions")
        | _ -> raise (Failure "compiler bug"))
      | _ -> raise (Failure "To be implemented"))
    in
    let rec build_toplevel = function
        SExpr (typ, exp) -> (match exp with
            SCall _ -> ignore (build_expr main_builder (typ, exp))
          | _ -> ())
      | SBind _ -> raise (Failure "to be implemented: bind")
      | SDeclClass _ -> raise (Failure "to be implemented: class declaration")
    in
    List.iter build_toplevel stoplevels; L.build_ret (L.const_int i32_t 0) main_builder
  in
  ignore (build_program sprogram); the_module
