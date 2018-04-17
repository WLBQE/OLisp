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
      | SCall ((SBuiltInTyp Print, SBuiltIn Print), [exp]) ->
        L.build_call printf_func [| int_format_str ; (expr builder exp) |] "printf" builder
      | _ -> raise (Failure "To be implemented")
    in
    let rec toplevel builder = function
        SExpr e -> ignore(expr builder e); L.build_ret_void builder
      | s -> raise (Failure "To be implemented")
    in ignore(toplevel builder top)
  in List.iter build_toplevel stoplevels;
  the_module
