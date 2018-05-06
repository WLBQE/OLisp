module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (sym_semant, cls, stoplevels) =
  let context  = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context in
  let double_t = L.double_type context in
  let void_t = L.void_type context in
  let void_ptr_t = L.pointer_type i8_t in
  let list_struct_t = L.named_struct_type context "list" in
  let () = L.struct_set_body list_struct_t [|void_ptr_t; L.pointer_type list_struct_t|] false in
  let the_module = L.create_module context "OLisp" in
  let structs =
    let add_struct mp (name, (_, constrlist)) =
      let add_member (mp, i) member = StringMap.add member i mp, i + 1 in
      let cmap, _ = List.fold_left add_member (StringMap.empty, 0) constrlist in
      StringMap.add name ((L.named_struct_type context name), cmap) mp
    in
    List.fold_left add_struct StringMap.empty (StringMap.bindings cls)
  in
  let rec ltype_of_styp = function
      SInt -> i32_t
    | SDouble -> double_t
    | SBool -> i1_t
    | SString -> L.pointer_type i8_t
    | SLambda (typs, ret) ->
      L.pointer_type (L.function_type (ltype_of_sret_typ ret) (Array.of_list (List.map ltype_of_styp typs)))
    | SClass name -> L.pointer_type (fst (StringMap.find name structs))
    | SList _ -> L.pointer_type list_struct_t
  and
  ltype_of_sret_typ = function
      SVarType typ -> ltype_of_styp typ
    | SVoid -> void_t
    | SBuiltInTyp _ -> raise (Failure "compiler bug")
  in
  let () =
    let add_class name =
      let vars, constrlist = StringMap.find name cls in
      let var_types = List.map ltype_of_styp (List.map (fun var_name -> StringMap.find var_name vars) constrlist) in
      L.struct_set_body (L.element_type (ltype_of_styp (SClass name))) (Array.of_list var_types) false
    in
    List.iter add_class (fst (List.split (StringMap.bindings cls)))
  in
  let global_vars =
    let get_init_val typ = L.const_null (ltype_of_styp typ) in
    let add_global_var mp (name, typ) = StringMap.add name (L.define_global name (get_init_val typ) the_module) mp in
    List.fold_left add_global_var StringMap.empty (StringMap.bindings sym_semant)
  in
  let rec lookup name = function
      var :: rest -> (try StringMap.find name var with Not_found -> lookup name rest)
    | [] -> raise (Failure "compiler bug")
  in
  let build_program stoplevels =
    let main_t = L.function_type i32_t [||] in
    let main_func = L.define_function "main" main_t the_module in
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in
    let strcmp_t = L.function_type i32_t [|L.pointer_type i8_t; L.pointer_type i8_t|] in
    let strcmp_func = L.declare_function "strcmp" strcmp_t the_module in
    let make_list_t = L.var_arg_function_type (L.pointer_type list_struct_t) [|i32_t|] in
    let make_list_func = L.declare_function "make_list" make_list_t the_module in
    let cons_t = L.function_type void_ptr_t [|void_ptr_t; L.pointer_type list_struct_t|] in
    let cons_func = L.declare_function "cons" cons_t the_module in
    let car_t = L.function_type void_ptr_t [|L.pointer_type list_struct_t|] in
    let car_func = L.declare_function "car" car_t the_module in
    let cdr_t = L.function_type (L.pointer_type list_struct_t) [|L.pointer_type list_struct_t|] in
    let cdr_func = L.declare_function "cdr" cdr_t the_module in
    let append_t = L.var_arg_function_type (L.pointer_type list_struct_t) [|i32_t|] in
    let append_func = L.declare_function "append" append_t the_module in
    let builder = L.builder_at_end context (L.entry_block main_func) in
    let int_format_str = L.build_global_stringptr "%d\n" "i_fmt" builder in
    let double_format_str = L.build_global_stringptr "%g\n" "f_fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "s_fmt" builder in
    let true_str = L.build_global_stringptr "true\n" "true" builder in
    let false_str = L.build_global_stringptr "false\n" "false" builder in
    let bool_str = L.define_global "bool" (L.const_array (L.pointer_type i8_t) [|false_str; true_str|]) the_module in
    let rec build_expr builder env (t, e) = match e with
        SLit i -> L.const_int i32_t i
      | SDoubleLit d -> L.const_float_of_string double_t d
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SId id -> let _, sym = env in L.build_load (lookup id sym) id builder
      | SMemId ((first, first_typ), middle, last) ->
        let _, sym = env in
        let members, class_names = List.split middle in
        let first_ptr = L.build_load (lookup first sym) first builder in
        let struct_idx_of_mem member class_name = StringMap.find member (snd (StringMap.find class_name structs)) in
        let process_name ptr member class_name =
          let index = struct_idx_of_mem member class_name in
          let new_ptr = L.build_struct_gep ptr index (class_name ^ "." ^ member) builder in
          L.build_load new_ptr (class_name ^ "." ^ member) builder
        in
        List.fold_left2 process_name first_ptr (members @ [last]) (first_typ :: class_names)
      | SLst (typ, exprs) ->
        let add_arg expr =
          let arg = build_expr builder env expr in
          let ptr = L.build_malloc (L.type_of arg) "" builder in
          let _ = L.build_store arg ptr builder in
          ptr
        in
        let ptrs = List.map add_arg exprs in
        let length = L.const_int i32_t (List.length exprs) in
        let call = L.build_call make_list_func (Array.of_list (length :: ptrs)) "list" builder in
        L.build_pointercast call (ltype_of_sret_typ t) "list" builder
      | SLambdaExpr (typs, ret, formals, expr) ->
        let lamb_func = L.define_function "lambda" (L.element_type (ltype_of_sret_typ t)) the_module in
        let lamb_builder = L.builder_at_end context (L.entry_block lamb_func) in
        let params = Array.to_list (L.params lamb_func) in
        let add_formal mp (typ, name) param =
          let local = L.build_alloca (ltype_of_styp typ) name lamb_builder in
          let _ = L.build_store param local lamb_builder in
          StringMap.add name local mp
        in
        let sym' = List.fold_left2 add_formal StringMap.empty (List.combine typs formals) params in
        let _ =
          let _, sym = env in
          match ret with
            SVoid -> let _ = build_expr lamb_builder (lamb_func, (sym' :: sym)) expr in L.build_ret_void lamb_builder
          | _ -> L.build_ret (build_expr lamb_builder (lamb_func, (sym' :: sym)) expr) lamb_builder
        in
        lamb_func
      | SCall (lamb, exprs) -> (match lamb with
          (SBuiltInTyp builtin, _) -> (match builtin with
            A.Add | A.Mult->
            let typ, _ = List.hd exprs in
            let linstr = match (typ, builtin) with
                SVarType SInt, A.Add -> L.build_add
              | SVarType SDouble, A.Add -> L.build_fadd
              | SVarType SInt, A.Mult -> L.build_mul
              | SVarType SDouble, A.Mult -> L.build_fmul
              | _ -> raise (Failure "compiler bug")
            in
            let rec build_multivar = function
                hd :: [e] ->
                let arg1 = build_expr builder env hd in linstr arg1 (build_expr builder env e) "val" builder
              | hd :: (h :: tl) ->
                let arg1 = build_expr builder env hd in linstr arg1 (build_multivar (h :: tl)) "val" builder
              | _ -> raise (Failure "compiler bug")
            in
            build_multivar exprs
          | A.And | A.Or->
            let func, _ = env in
            let is_and = builtin = A.And in
            let rec build_preds builder merge result = function
                [] ->
                let _ = L.build_store (L.const_int i1_t (if is_and then 1 else 0)) result builder in
                L.build_br merge builder
              | hd :: tl ->
                let hd_val = build_expr builder env hd in
                let next = L.append_block context "pred" func in
                let _ = if is_and then L.build_cond_br hd_val next merge builder
                  else L.build_cond_br hd_val merge next builder
                in
                build_preds (L.builder_at_end context next) merge result tl
            in
            let result = L.build_alloca i1_t "result" builder in
            let _ = L.build_store (L.const_int i1_t (if is_and then 0 else 1)) result builder in
            let merge = L.append_block context "merge" func in
            let _ = build_preds builder merge result exprs in
            let () = L.position_builder (L.instr_begin merge) builder in
            L.build_load result "result" builder
          | A.Sub | A.Div | A.Mod | A.Eq | A.Neq | A.Lt | A.Gt | A.Leq | A.Geq ->
            let typ, _ = List.hd exprs in
              (match typ with
                SVarType SInt | SVarType SDouble | SVarType SBool ->
                let linstr = match typ, builtin with
                    SVarType SInt, A.Sub -> L.build_sub
                  | SVarType SDouble, A.Sub -> L.build_fsub
                  | SVarType SInt, A.Div -> L.build_sdiv
                  | SVarType SDouble, A.Div -> L.build_fdiv
                  | SVarType SInt, A.Mod -> L.build_srem
                  | SVarType SInt, A.Eq | SVarType SBool, A.Eq -> L.build_icmp L.Icmp.Eq
                  | SVarType SDouble, A.Eq -> L.build_fcmp L.Fcmp.Oeq
                  | SVarType SInt, A.Neq | SVarType SBool, A.Neq -> L.build_icmp L.Icmp.Ne
                  | SVarType SDouble, A.Neq -> L.build_fcmp L.Fcmp.One
                  | SVarType SInt, A.Lt -> L.build_icmp L.Icmp.Slt
                  | SVarType SDouble, A.Lt -> L.build_fcmp L.Fcmp.Olt
                  | SVarType SInt, A.Gt -> L.build_icmp L.Icmp.Sgt
                  | SVarType SDouble, A.Gt -> L.build_fcmp L.Fcmp.Ogt
                  | SVarType SInt, A.Leq -> L.build_icmp L.Icmp.Sle
                  | SVarType SDouble, A.Leq -> L.build_fcmp L.Fcmp.Ole
                  | SVarType SInt, A.Geq -> L.build_icmp L.Icmp.Sge
                  | SVarType SDouble, A.Geq -> L.build_fcmp L.Fcmp.Oge
                  | _ -> raise (Failure "compiler bug")
                in
                let arg1 = build_expr builder env (List.hd exprs) in
                let arg2 = build_expr builder env (List.hd (List.tl exprs)) in
                linstr arg1 arg2 "val" builder
              | SVarType SString ->
                let comp = match builtin with
                  A.Eq -> L.build_icmp L.Icmp.Eq
                | A.Neq -> L.build_icmp L.Icmp.Ne
                | _ -> raise (Failure "compiler bug")
                in
                let arg1 = build_expr builder env (List.hd exprs) in
                let arg2 = build_expr builder env (List.hd (List.tl exprs)) in
                comp (L.const_int i32_t 0) (L.build_call strcmp_func [|arg1; arg2|] "strcmp" builder) "cmp" builder
              | _ -> raise (Failure "compiler bug"))
          | A.Not -> L.build_not (build_expr builder env (List.hd exprs)) "not" builder
          | A.I2d -> L.build_sitofp (build_expr builder env (List.hd exprs)) (ltype_of_styp SDouble) "double" builder
          | A.D2i -> L.build_fptosi (build_expr builder env (List.hd exprs)) (ltype_of_styp SInt) "int" builder
          | A.Cons ->
            let hd_val = build_expr builder env (List.hd exprs) in
            let hd_ptr = L.build_malloc (L.type_of hd_val) "" builder in
            let _ = L.build_store hd_val hd_ptr builder in
            let hd = L.build_pointercast hd_ptr void_ptr_t "" builder in
            let tl = build_expr builder env (List.hd (List.tl exprs)) in
            let ptr = L.build_call cons_func [|(hd); tl|] "cons" builder in
            L.build_pointercast ptr (ltype_of_sret_typ t) "cons" builder
          | A.Car ->
            let lst = build_expr builder env (List.hd exprs) in
            let ptr = L.build_call car_func [|lst|] "car" builder in
            let typ_ptr = L.build_pointercast ptr (L.pointer_type (ltype_of_sret_typ t)) "car" builder in
            L.build_load typ_ptr "car" builder
          | A.Cdr -> let lst = build_expr builder env (List.hd exprs) in L.build_call cdr_func [|lst|] "cdr" builder
          | A.Append ->
            let lists = List.map (build_expr builder env) exprs in
            let length = L.const_int i32_t (List.length lists) in
            L.build_call append_func (Array.of_list (length :: lists)) "append" builder
          | A.Empty -> L.build_is_null (build_expr builder env (List.hd exprs)) "empty" builder
          | A.If -> (match exprs with
              [pred; e_then; e_else] ->
              let func, _ = env in
              let bool_val = build_expr builder env pred in
              let result = match t with
                  SVoid -> L.const_int i1_t 0
                | _ -> L.build_alloca (ltype_of_sret_typ t) "result" builder
              in
              let merge_bb = L.append_block context "merge" func in
              let then_bb = L.append_block context "then" func in
              let then_builder = L.builder_at_end context then_bb in
              let then_expr = build_expr then_builder env e_then in
              let () = match t with SVoid -> () | _ -> ignore (L.build_store then_expr result then_builder) in
              let _ = L.build_br merge_bb then_builder in
              let else_bb = L.append_block context "else" func in
              let else_builder = L.builder_at_end context else_bb in
              let else_expr = build_expr else_builder env e_else in
              let () = match t with SVoid -> () | _ -> ignore (L.build_store else_expr result else_builder) in
              let _ = L.build_br merge_bb else_builder in
              let _ = L.build_cond_br bool_val then_bb else_bb builder in
              let () = L.position_builder (L.instr_begin merge_bb) builder in
              (match t with SVoid -> result | _ -> L.build_load result "val" builder)
            | _ -> raise (Failure "compiler bug"))
          | A.Begin -> List.hd (List.rev (List.map (build_expr builder env) exprs))
          | A.Print ->
            let e = List.hd exprs in
            let args = match e with
                SVarType SInt, _ -> [|int_format_str; build_expr builder env e|]
              | SVarType SDouble, _ -> [|double_format_str; build_expr builder env e|]
              | SVarType SBool, _ -> [|L.build_load (L.build_in_bounds_gep bool_str
                  [|L.const_int i32_t 0; L.build_zext (build_expr builder env e) i32_t "bool_str_idx" builder|]
                  "bool_str_ptr" builder) "bool_str" builder|]
              | SVarType SString, _ -> [|string_format_str; build_expr builder env e|]
              | _ -> raise (Failure "compiler bug")
            in
            L.build_call printf_func args "printf" builder)
        | SVarType (SLambda (_, ret)), _ ->
          let lamb' = build_expr builder env lamb in
          L.build_call lamb' (Array.of_list (List.map (build_expr builder env) exprs))
            (match ret with SVoid -> "" | _ -> "call") builder
        | _ -> raise (Failure "compiler bug"))
      | SBuiltIn _ -> raise (Failure "compiler bug")
    in
    let build_toplevel = function
        SExpr (typ, expr) -> build_expr builder (main_func, [global_vars]) (typ, expr)
      | SBind (typ, name, expr) ->
        let expr' = build_expr builder (main_func, [global_vars]) expr in
        L.build_store expr' (StringMap.find name global_vars) builder
      | SDeclClass (name, _, _) ->
        let vars, constrlist = StringMap.find name cls in
        let bindings = List.map (fun var_name -> (StringMap.find var_name vars, var_name)) constrlist in
        let constructor_type = L.element_type (L.element_type (L.type_of (StringMap.find name global_vars))) in
        let constructor = L.define_function ("constructor_" ^ name) constructor_type the_module in
        let constructor_builder = L.builder_at_end context (L.entry_block constructor) in
        let return_value = L.build_malloc (fst (StringMap.find name structs)) "return" constructor_builder in
        let params = Array.to_list (L.params constructor) in
        let add_member i (typ, name) param =
          let ptr = L.build_struct_gep return_value i name constructor_builder in
          let _ = L.build_store param ptr constructor_builder in
          i + 1
        in
        let _ = List.fold_left2 add_member 0 bindings params in
        let _ = L.build_ret return_value constructor_builder in
        L.build_store constructor (StringMap.find name global_vars) builder
    in
    let _ = List.map build_toplevel stoplevels in
    L.build_ret (L.const_int i32_t 0) builder
  in
  let _ = build_program stoplevels in
  the_module
