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
    | SLambda (typs, ret) -> L.pointer_type (L.function_type (ltype_of_sret_typ ret)
        (Array.of_list (List.map ltype_of_styp typs)))
    | SList typ -> raise (Failure "to be implemented: list")
    | SClass name -> raise (Failure "to be implemented: class")
  and
  ltype_of_sret_typ = function
      SVarType typ -> ltype_of_styp typ
    | SBuiltInTyp builtin -> raise (Failure "compiler bug")
    | SVoid -> void_t
  in
  let global_vars =
    let add_global_var mp (name, typ) =
      let init = match typ with
          SInt | SBool -> L.const_int (ltype_of_styp typ) 0
        | SDouble -> L.const_float (ltype_of_styp typ) 0.0
        | SString | SLambda _ -> L.const_null (ltype_of_styp typ)
        | _ -> raise (Failure "to be implemented")
      in
      StringMap.add name (L.define_global name init the_module) mp
    in
    List.fold_left add_global_var StringMap.empty (StringMap.bindings sym)
  in
  let rec lookup name = function
      [] -> raise Not_found
    | var :: rest -> try StringMap.find name var with Not_found -> lookup name rest
  in
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
  let strcmp_t = L.function_type i32_t [|L.pointer_type i8_t; L.pointer_type i8_t|] in
  let strcmp_func = L.declare_function "strcmp" strcmp_t the_module in
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
    let rec build_expr builder env (t, e) = (match e with
        SLit i -> L.const_int i32_t i
      | SDoubleLit d -> L.const_float_of_string double_t d
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SId id -> L.build_load (lookup id env) id builder
      | SLambdaExpr (typs, ret, formals, expr) -> let lamb_function =
          L.define_function "lambda" (L.element_type (ltype_of_sret_typ t)) the_module in
        let lamb_builder = L.builder_at_end context (L.entry_block lamb_function) in
        let params = Array.to_list (L.params lamb_function) in
        let add_formal mp (typ, name) param =
          let () = L.set_value_name name param in
          let local = L.build_alloca (ltype_of_styp typ) name lamb_builder in
          let _ = L.build_store param local lamb_builder in
          StringMap.add name local mp
        in
        let sym' = List.fold_left2 add_formal StringMap.empty (List.combine typs formals) params in
        let _ = (match ret with
            SVoid -> L.build_ret_void lamb_builder
          | _ -> L.build_ret (build_expr lamb_builder (sym' :: env) expr) lamb_builder)
        in
        lamb_function
      | SCall (lamb, exprs) -> (match lamb with
          (SBuiltInTyp builtin, _) -> (match builtin with
            Add | Mult | And | Or -> let (typ, _) = List.hd exprs in
            let linstr = (match (typ, builtin) with
                SVarType SInt, Add -> L.build_add
              | SVarType SDouble, Add -> L.build_fadd
              | SVarType SInt, Mult -> L.build_mul
              | SVarType SDouble, Mult -> L.build_fmul
              | SVarType SBool, And -> L.build_and
              | SVarType SBool, Or -> L.build_or
              | _ -> raise (Failure "compiler bug"))
            in
            let rec build_multivar = (function
                hd :: [e] -> let arg1 = (build_expr builder env hd) in
                linstr arg1 (build_expr builder env e) "val" builder
              | hd :: (h :: t) -> let arg1 = (build_expr builder env hd) in
                linstr arg1 (build_multivar (h :: t)) "val" builder
              | _ -> raise (Failure "compiler bug"))
            in
            build_multivar exprs
          | Sub | Div | Mod | Eq | Neq | Lt | Gt | Leq | Geq ->
            let (typ, _) = List.hd exprs in (match typ with
                SVarType SInt | SVarType SDouble | SVarType SBool ->
                let linstr = (match (typ, builtin) with
                    SVarType SInt, Sub -> L.build_sub
                  | SVarType SDouble, Sub -> L.build_fsub
                  | SVarType SInt, Div -> L.build_sdiv
                  | SVarType SDouble, Div -> L.build_fdiv
                  | SVarType SInt, Mod -> L.build_srem
                  | SVarType SInt, Eq | SVarType SBool, Eq -> L.build_icmp L.Icmp.Eq
                  | SVarType SDouble, Eq -> L.build_fcmp L.Fcmp.Oeq
                  | SVarType SInt, Neq | SVarType SBool, Neq -> L.build_icmp L.Icmp.Ne
                  | SVarType SDouble, Neq -> L.build_fcmp L.Fcmp.One
                  | SVarType SInt, Lt -> L.build_icmp L.Icmp.Slt
                  | SVarType SDouble, Lt -> L.build_fcmp L.Fcmp.Olt
                  | SVarType SInt, Gt -> L.build_icmp L.Icmp.Sgt
                  | SVarType SDouble, Gt -> L.build_fcmp L.Fcmp.Ogt
                  | SVarType SInt, Leq -> L.build_icmp L.Icmp.Sle
                  | SVarType SDouble, Leq -> L.build_fcmp L.Fcmp.Ole
                  | SVarType SInt, Geq -> L.build_icmp L.Icmp.Sge
                  | SVarType SDouble, Geq -> L.build_fcmp L.Fcmp.Oge
                  | _ -> raise (Failure "compiler bug"))
                in
                (function
                    [e1; e2] -> let arg1 = (build_expr builder env e1) in
                    linstr arg1 (build_expr builder env e2) "val" builder
                  | _ -> raise (Failure "compiler bug")) exprs
              | SVarType SString -> let comp = (match builtin with
                  Eq -> L.build_icmp L.Icmp.Eq
                | Neq -> L.build_icmp L.Icmp.Ne
                | _ -> raise (Failure "compiler bug"))
                in
                (function
                    [e1; e2] -> comp (L.const_int i32_t 0) (L.build_call strcmp_func
                      [|(build_expr builder env e1); (build_expr builder env e2)|] "strcmp" builder) "cmp" builder
                  | _ -> raise (Failure "compiler bug")) exprs
              | _ -> raise (Failure "compiler bug"))
          | I2d -> L.build_sitofp (build_expr builder env (List.hd exprs))
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
        | (SVarType (SLambda _), _) -> let lamb' = (build_expr builder env lamb) in
          L.build_call lamb' (Array.of_list (List.map (build_expr builder env) exprs)) "call" builder
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
