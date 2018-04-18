module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)


let INIT_LIST_SIZE = 5
let counter = ref 0

let translate (sym, cls, stoplevels) =
  let context  = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context
  let float_t = L.double_type context
  let intlistnode_t = L.struct_type context [| i32_t; L.pointer_type i32_t |]
  let floatlistnode_t = L.struct_type context [| float_t; L.pointer_type float_t |]
  let boollistnode_t = L.struct_type context [| i1_t; L.pointer_type i1_t |]
  let strlistnode_t = L.struct_type context [| i8_t; L.pointer_type i8_t |]
  let void_t = L.void_type context in
  let the_module = L.create_module context "OLisp" in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Double -> float_t
    | A.Void -> void_t
    | A.List(typ) -> L.array_type (ltype_of_typ typ) INIT_LIST_SIZE

(*  let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in
  (* not sure why the fuck i32_t should be the return type of print_t*)
  let add_t : L.lltype = L.var_arg_function_type i32_t [| intlistnode_t |] in
  let add_func : L.llvalue = L.declare_function "add" add_t the_module in

  let sub_t : L.lltype = L.var_arg_function_type i32_t [| intlistnode_t |] in
  let sub_func : L.llvalue = L.declare_function "sub" sub_t the_module in *)


  let functions =
    List.fold_left
    fun lst checked_item -> match checked with
        SBind(typ, name, expr) -> match typ with
                                  SLambda(_,_) -> (name, expr) :: lst
                                | _ -> lst
      | SLambdaExpr(typ_list, ret_typ, formal_list, expr) ->
        let getCount _ =
          counter := counter + 1
        in ("lambda" ^ string_of_int (getCount ""), SLambdaExpr(typ_list, ret_typ, formal_list, expr))
    []
    checked

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl = match fdecl with
        (name, SLambdaExpr(typ_list, ret_typ, formal_list, expr)) ->
          let formal_types = Array.of_list
            (List.map (fun (t, _) -> ltype_of_typ t) formal_list) in
          let ftype = L.function_type (ltype_of_typ ret_typ) typ_list in
          let llvm_func = L.define_function name ftype the_module in
          StringMap.add name (llvm_func, fdecl) m
        | _ -> m
    in
    List.foid_left function_decl StringMap.empty functions in

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
