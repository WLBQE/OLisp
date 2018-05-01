module L = Llvm
open Ast
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
  let structs = List.fold_left (fun mp (name, _) ->
    StringMap.add name (L.named_struct_type context name) mp) StringMap.empty (StringMap.bindings cls)
  in
  let rec ltype_of_styp = function
      SInt -> i32_t
    | SDouble -> double_t
    | SBool -> i1_t
    | SString -> L.pointer_type i8_t
    | SLambda (typs, ret) ->
      L.pointer_type (L.function_type (ltype_of_sret_typ ret) (Array.of_list (List.map ltype_of_styp typs)))
    | SClass name -> L.pointer_type (StringMap.find name structs)
    | SList typ -> raise (Failure "to be implemented: list")
  and
  ltype_of_sret_typ = function
      SVarType typ -> ltype_of_styp typ
    | SBuiltInTyp _ -> raise (Failure "compiler bug")
    | SVoid -> void_t
  in
  let () =
    let add_class name =
      let (vars, constrlist) = StringMap.find name cls in
      let var_types = List.map ltype_of_styp (List.map (fun var_name -> StringMap.find var_name vars) constrlist) in
      L.struct_set_body (L.element_type (ltype_of_styp (SClass name))) (Array.of_list var_types) false
    in
    List.iter add_class (fst (List.split (StringMap.bindings cls)))
  in
  let global_vars =
    let rec get_init_val typ = match typ with
        SInt | SBool -> L.const_int (ltype_of_styp typ) 0
      | SDouble -> L.const_float (ltype_of_styp typ) 0.0
      | SString | SLambda _ | SClass _ -> L.const_null (ltype_of_styp typ)
      | SList _ -> raise (Failure "to be implemented: list")
    in
    let add_global_var mp (name, typ) = StringMap.add name (L.define_global name (get_init_val typ) the_module) mp in
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
  let build_program (sym, stoplevels) =
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
      | SId id -> let (_, sym) = env in L.build_load (lookup id sym) id builder
      | SMemId (names, name) -> raise (Failure "to be implemented: class")
      | SLst (typ, exprs) -> raise (Failure "to be implemented: list")
      | SLambdaExpr (typs, ret, formals, expr) -> let lamb_function =
          L.define_function "lambda" (L.element_type (ltype_of_sret_typ t)) the_module in
        let lamb_builder = L.builder_at_end context (L.entry_block lamb_function) in
        let params = Array.to_list (L.params lamb_function) in
        let add_formal mp (typ, name) param =
          let local = L.build_alloca (ltype_of_styp typ) name lamb_builder in
          let _ = L.build_store param local lamb_builder in
          StringMap.add name local mp
        in
        let sym' = List.fold_left2 add_formal StringMap.empty (List.combine typs formals) params in
        let _ = let (_, sym) = env in (match ret with
            SVoid -> ignore (build_expr lamb_builder (lamb_function, (sym' :: sym)) expr);
            L.build_ret_void lamb_builder
          | _ -> L.build_ret (build_expr lamb_builder (lamb_function, (sym' :: sym)) expr) lamb_builder)
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
              | hd :: (h :: tl) -> let arg1 = (build_expr builder env hd) in
                linstr arg1 (build_multivar (h :: tl)) "val" builder
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
          | Not -> L.build_not (build_expr builder env (List.hd exprs)) "not" builder
          | I2d -> L.build_sitofp (build_expr builder env (List.hd exprs))
              (ltype_of_styp SDouble) "double" builder
          | D2i -> L.build_fptosi (build_expr builder env (List.hd exprs))
              (ltype_of_styp SInt) "int" builder
          | Cons | Car | Cdr | Append | Empty -> raise (Failure "to be implemented: list")
          | If -> (match exprs with
              [pred; e_then; e_else] -> let (func, _) = env in
              let bool_val = build_expr builder env pred in
              let result = (match t with
                  SVoid -> L.const_int i1_t 0
                | _ -> L.build_alloca (ltype_of_sret_typ t) "result" builder)
              in
              let merge_bb = L.append_block context "merge" func in
              let then_bb = L.append_block context "then" func in
              let then_builder = L.builder_at_end context then_bb in
              let then_expr = build_expr then_builder env e_then in
              let () = (match t with
                  SVoid -> ()
                | _ -> ignore (L.build_store then_expr result then_builder))
              in
              let _ = L.build_br merge_bb then_builder in
              let else_bb = L.append_block context "else" func in
              let else_builder = L.builder_at_end context else_bb in
              let else_expr = build_expr else_builder env e_else in
              let () = (match t with
                  SVoid -> ()
                | _ -> ignore (L.build_store else_expr result else_builder))
              in
              let _ = L.build_br merge_bb else_builder in
              let _ = L.build_cond_br bool_val then_bb else_bb builder in
              let () = L.position_builder (L.instr_begin merge_bb) builder in
              (match t with
                  SVoid -> result
                | _ -> L.build_load result "val" builder)
            | _ -> raise (Failure "compiler bug"))
          | Begin -> List.hd (List.rev (List.map (build_expr builder env) exprs))
          | Print -> L.build_call printf_func (let e = List.hd exprs in match e with
                (SVarType SInt, _) -> [|int_format_str; build_expr builder env e|]
              | (SVarType SDouble, _) -> [|double_format_str; build_expr builder env e|]
              | (SVarType SBool, _) -> [|L.build_load (L.build_in_bounds_gep bool_str
                  [|L.const_int i32_t 0; L.build_zext (build_expr builder env e) i32_t "bool_str_idx" builder|]
                  "bool_str_ptr" builder) "bool_str" builder|]
              | (SVarType SString, _) -> [|string_format_str; build_expr builder env e|]
              | _ -> raise (Failure "compiler bug"))
            "printf" builder)
        | (SVarType (SLambda (_, ret)), _) -> let lamb' = (build_expr builder env lamb) in
          L.build_call lamb' (Array.of_list (List.map (build_expr builder env) exprs))
            (match ret with SVoid -> "" | _ -> "call") builder
        | _ -> raise (Failure "compiler bug"))
      | SBuiltIn _ -> raise (Failure "compiler bug"))
    in
    let rec build_toplevel = function
        SExpr (typ, expr) -> (match expr with
            SCall _ -> ignore (build_expr builder (main_function, [global_vars]) (typ, expr))
          | _ -> ())
      | SBind (typ, name, expr) -> let expr' = build_expr builder (main_function, [global_vars]) expr in
        ignore (L.build_store expr' (StringMap.find name global_vars) builder)
      | SDeclClass (name, _, _) -> let (vars, constrlist) = StringMap.find name cls in
        let bindings = List.map (fun var_name -> (StringMap.find var_name vars, var_name)) constrlist in
        let constructor_type = (L.element_type (L.element_type (L.type_of (StringMap.find name global_vars)))) in
        let constructor = L.define_function ("constructor_" ^ name) constructor_type the_module in
        let constructor_builder = L.builder_at_end context (L.entry_block constructor) in
        let return_value = L.build_malloc (StringMap.find name structs) "return" constructor_builder in
        let params = Array.to_list (L.params constructor) in
        let add_member i (typ, name) param =
          let ptr = L.build_struct_gep return_value i name constructor_builder in
          let _ = L.build_store param ptr constructor_builder in
          i + 1
        in
        let _ = List.fold_left2 add_member 0 bindings params in
        let _ = L.build_ret return_value constructor_builder in
        ignore (L.build_store constructor (StringMap.find name global_vars) builder)
    in
    let () = List.iter build_toplevel stoplevels in
    L.build_ret (L.const_int i32_t 0) builder
  in
  ignore (build_program (sym, stoplevels)); the_module
