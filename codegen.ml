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
