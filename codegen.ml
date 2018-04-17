module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (sym, cls, stoplevels) =
  let context  = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let void_t = L.void_type context in
  let the_module = L.create_module context "OLisp" in

  let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in

  let build_toplevel top =
    let main_ty = L.function_type void_t [||] in
    let the_function = L.define_function "main" main_ty the_module in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "i_fmt" builder in
    let rec expr builder ((_, e) : sexpr) = match e with
        SLit i -> L.const_int i32_t i
      | SDoubleLit l -> L.const_float_of_string float_t l
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      (*| SStringLit -> 
      | SId s -> L.build_load (lookup s) s builder 
      | SMemId*)
      | SCall ((SBuiltInTyp Print, SBuiltIn Print), [exp]) ->
        L.build_call printf_func [| int_format_str ; (expr builder exp) |] "printf" builder
      (*| SCall ((SBuiltInTyp Add, SBuiltIn Add), [exp]) ->

      | SCall ((SBuiltInTyp Sub, SBuiltIn Sub), [exp]) ->

      | SCall ((SBuiltInTyp Mult, SBuiltIn Mult), [exp]) ->

      | SCall ((SBuiltInTyp Div, SBuiltIn Div), [exp]) ->

      | SCall ((SBuiltInTyp Mod, SBuiltIn Mod), [exp]) ->*)
      | SCall ((SBuiltInTyp EQ, SBuiltIn EQ), e1, e2) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = A.Double then L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp Neq, SBuiltIn Neq), e1, e2) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = A.Double then L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp Lt, SBuiltIn Lt), e1, e2) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = A.Double then L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder 
      | SCall ((SBuiltInTyp Gt, SBuiltIn Gt), e1, e2) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = A.Double then L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp Leq, SBuiltIn Leq), e1, e2) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = A.Double then L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
      | SCall ((SBuiltInTyp Geq, SBuiltIn Geq), e1, e2) ->
        let (t, _) = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
        if t = A.Double then L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
        else L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
      (*| SCall (f, args) -> let (fdef, fdecl) = StringMap.find f 
      | SLst
      | SLambdaExpr *)
      | _ -> raise (Failure "To be implemented")
    in
    let rec toplevel builder = function
        SExpr e -> ignore(expr builder e); L.build_ret_void builder
      | s -> raise (Failure "To be implemented")
    in ignore(toplevel builder top)
  in List.iter build_toplevel stoplevels;
  the_module
    
