module L = Llvm
open Sast

module StringMap = Map.Make(String)

let translate (sym, cls, stoplevels) =
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
  let global_vars =
    let add_global_var mp (name, typ) =
      let init = match typ with
          SDouble -> L.const_float (ltype_of_styp typ) 0.0
        | SString -> L.const_null (ltype_of_styp typ)
        | _ -> L.const_int (ltype_of_styp typ) 0
      in
      StringMap.add name (L.define_global name init the_module) mp in
    List.fold_left add_global_var StringMap.empty (StringMap.bindings sym)
  in
  let rec lookup name = function
      [] -> raise Not_found
    | var :: rest -> try StringMap.find name var with Not_found -> lookup name rest
  in
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
  let build_program (sym, cls, stoplevels) =
    let main_ty = L.function_type i32_t [||] in
    let main_function = L.define_function "main" main_ty the_module in
    let builder = L.builder_at_end context (L.entry_block main_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "i_fmt" builder in
    let double_format_str = L.build_global_stringptr "%g\n" "f_fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "s_fmt" builder in
    let true_str = L.build_global_stringptr "true\n" "true" builder in
    let false_str = L.build_global_stringptr "false\n" "false" builder in
    let bool_str = L.define_global "bool"
      (L.const_array (L.pointer_type i8_t) [|false_str; true_str|]) the_module in
    let rec build_expr builder env (_, e) = (match e with
        SLit i -> L.const_int i32_t i
      | SDoubleLit d -> L.const_float_of_string double_t d
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SId id -> L.build_load (lookup id env) id builder
      | SCall (lamb, exprs) -> (match lamb with
          (SBuiltInTyp builtin, _) -> (match builtin with
            I2d -> L.build_sitofp (build_expr builder env (List.hd exprs))
              (ltype_of_styp SDouble) "double" builder
          | D2i -> L.build_fptosi (build_expr builder env (List.hd exprs))
              (ltype_of_styp SInt) "int" builder
          | Begin -> List.hd (List.rev (List.map (build_expr builder env) exprs))
          | Print -> L.build_call printf_func (let e = List.hd exprs in match e with
                (SVarType SInt, _) -> [|int_format_str; build_expr builder env e|]
              | (SVarType SDouble, _) -> [|double_format_str; build_expr builder env e|]
              | (SVarType SBool, _) -> [|L.build_load (L.build_in_bounds_gep bool_str
                  [|L.const_int i32_t 0; L.build_zext (build_expr builder env e) i32_t "bool_str_idx" builder|]
                  "bool_str_ptr" builder) "bool_str" builder|]
              | (SVarType SString, _) -> [|string_format_str; build_expr builder env e|]
              | _ -> raise (Failure "compiler bug"))
            "printf" builder
          | _ -> raise (Failure "to be implemented: built-in"))
        | (SVarType (SLambda _), _) -> raise (Failure "to be implemented: lambda expressions")
        | _ -> raise (Failure "compiler bug"))
      | _ -> raise (Failure "To be implemented"))
    in
    let rec build_toplevel = function
        SExpr (typ, expr) -> (match expr with
            SCall _ -> ignore (build_expr builder [global_vars] (typ, expr))
          | _ -> ())
      | SBind (typ, name, expr) -> let expr' = build_expr builder [global_vars] expr in
        ignore (L.build_store expr' (StringMap.find name global_vars) builder)
      | SDeclClass _ -> raise (Failure "to be implemented: class declaration")
    in
    List.iter build_toplevel stoplevels; L.build_ret (L.const_int i32_t 0) builder
  in
  ignore (build_program (sym, cls, stoplevels)); the_module
