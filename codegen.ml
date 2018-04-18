module L = Llvm
module A = Ast
module S = Sast
open Sast

module StringMap = Map.Make(String)

let translate (sym, cls, stoplevels) =
  let counter = ref 0 in
  let init_list_size = 5 in
  let context  = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context in
  let float_t = L.double_type context in
(*  let intlistnode_t = L.struct_type context [| i32_t; L.pointer_type i32_t |] in
  let floatlistnode_t = L.struct_type context [| float_t; L.pointer_type float_t |] in
  let boollistnode_t = L.struct_type context [| i1_t; L.pointer_type i1_t |] in
  let strlistnode_t = L.struct_type context [| i8_t; L.pointer_type i8_t |] in *)
  let void_t = L.void_type context in
  let the_module = L.create_module context "OLisp" in

  let rec ltype_of_typ = function
      S.SInt -> i32_t
    | S.SBool -> i1_t
    | S.SDouble -> float_t
    | S.SList(typ) -> L.array_type (ltype_of_typ typ) init_list_size in

  let globals =
    let get_global lst checked_item = match checked_item with
        SBind(typ, name, expr) -> (match typ with
                                    SLambda(_,_) -> lst
                                  | _ -> (typ, name) :: lst)
      | _ -> lst
    in List.fold_left get_global [] stoplevels
  in

  let global_vars =
    let global_var m (typ, name) =
      let init = match typ with
          S.SDouble -> L.const_float (ltype_of_typ typ) 0.0
        | _ -> L.const_int (ltype_of_typ typ) 0
      in StringMap.add name (L.define_global name init the_module) m in
    List.fold_left global_var StringMap.empty globals
  in

  let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in
  (* not sure why the fuck i32_t should be the return type of print_t*)
  (*let add_t : L.lltype = L.var_arg_function_type i32_t [| intlistnode_t |] in
  let add_func : L.llvalue = L.declare_function "add" add_t the_module in

  let sub_t : L.lltype = L.var_arg_function_type i32_t [| intlistnode_t |] in
  let sub_func : L.llvalue = L.declare_function "sub" sub_t the_module in *)

  let functions =
    let get_binding lst checked_item = match checked_item with
        SBind(typ, name, expr) -> (match typ with
                                  SLambda(_,_) -> match expr with (_, sx) -> (name, sx) :: lst)
      | SExpr(sexpr) -> match sexpr with
                       (_, sx) -> match sx with
                                 SLambdaExpr(typ_list, ret_typ, formal_list, expr) ->
                                   let getCount _ =
                                     counter := !counter + 1;
                                     !counter
                                   in ("lambda" ^ string_of_int (getCount ""), SLambdaExpr(typ_list, ret_typ, formal_list, expr)) :: lst
                                | _ -> lst
    in
    List.fold_left get_binding [] stoplevels
  in

  let function_decls =
    let function_decl m fdecl = match fdecl with
        (name, SLambdaExpr(typ_list, ret_typ, formal_list, expr)) ->
          let formal_types = Array.of_list
            (List.map (fun typ -> ltype_of_typ typ) typ_list) in
          match ret_typ with
            SVarType(styp) ->
              let ftype = L.function_type (ltype_of_typ styp) formal_types in
              let llvm_func = L.define_function name ftype the_module in
              StringMap.add name (llvm_func, fdecl) m
          | _ -> m
    in
    List.fold_left function_decl StringMap.empty functions in

  let build_function_body fdecl = match fdecl with
      (name, SLambdaExpr(typ_list, ret_typ, formal_list, expr)) ->
        let (the_function, _) =
          StringMap.find name function_decls in
        let builder =
          L.builder_at_end context (L.entry_block the_function) in
        let int_format_str =
          L.build_global_stringptr "%d\n" "fmt" builder
        and float_format_str =
          L.build_global_stringptr "%g\n" "fmt" builder in
        let add_formals m (typ, name) param = L.set_value_name name param;
          let local = L.build_alloca (ltype_of_typ typ) name builder in
          let _ = L.build_store param local builder in StringMap.add name local m in
        let formal_intact_list =
          let zip_typ_name lst typ name = (typ, name) :: lst in
          List.fold_left2 zip_typ_name [] typ_list formal_list in
        List.fold_left2 add_formals StringMap.empty formal_intact_list (Array.to_list (L.params the_function))
      | _ -> raise (Failure ("You Fucked Up"))
  in

  let build_toplevel top =
    let main_ty = L.function_type void_t [||] in
    let the_function = L.define_function "main" main_ty the_module in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "i_fmt" builder in
    let rec expr builder ((_, e) : sexpr) = match e with
        SLit i -> L.const_int i32_t i
      (*| SDoubleLit l -> L.const_float_of_string float_t l*)
      | SBoolLit b -> L.const_int i8_t (if b then 1 else 0)
      (*| SStringLit ->*) 
      (*| SId s -> L.build_load (lookup s) s builder 
      | SMemId*)
      | SCall ((SBuiltInTyp A.Print, SBuiltIn A.Print), [exp]) ->
        L.build_call printf_func [| int_format_str ; (expr builder exp) |] "printf" builder
      | SCall ((SBuiltInTyp A.Add, SBuiltIn A.Add), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fadd e1' e2' "tmp" builder
        else L.build_add e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp A.Sub, SBuiltIn A.Sub), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fsub e1' e2' "tmp" builder
        else L.build_sub e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp A.Mult, SBuiltIn A.Mult), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fmul e1' e2' "tmp" builder
        else L.build_mul e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp A.Div, SBuiltIn A.Div), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fdiv e1' e2' "tmp" builder
        else L.build_sdiv e1' e2' "tmp" builder
      (*| SCall ((SBuiltInTyp A.Mod, SBuiltIn A.Mod), [e1; e2]) ->
        let e1' = expr builder e1 and e2' = expr builder e2 in
        L.builder.build_srem e1' e2' "tmp" builder*)
      | SCall ((SBuiltInTyp A.Eq, SBuiltIn A.Eq), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp A.Neq, SBuiltIn A.Neq), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp A.Lt, SBuiltIn A.Lt), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder 
      | SCall ((SBuiltInTyp A.Gt, SBuiltIn A.Gt), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp A.Leq, SBuiltIn A.Leq), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp A.Geq, SBuiltIn A.Geq), [e1; e2]) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = SVarType SDouble then L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
      | _ -> raise (Failure "To be implemented")
    in
    let rec toplevel builder = function
        SExpr e -> ignore(expr builder e); L.build_ret_void builder
      | s -> raise (Failure "To be implemented")
    in ignore(toplevel builder top)
  in List.iter build_toplevel stoplevels;
  the_module
    
