open Ast
open Sast

module StringMap = Map.Make(String)

let check toplevels =
  let confirm_type typ (ret, _) =
    if ret = SVarType typ then typ else
      raise (Failure (string_of_sret_typ ret ^ " and " ^ string_of_styp typ ^ " do not match"))
  in
  let confirm_ret_type ret_typ (ret, _) =
    if ret = ret_typ then ret_typ else
      raise (Failure (string_of_sret_typ ret ^ " and " ^ string_of_sret_typ ret_typ ^ " do not match"))
  in
  let rec check_type cls = function
      Int -> SInt
    | Double -> SDouble
    | Bool -> SBool
    | String -> SString
    | List typ -> SList (check_type cls typ)
    | Lambda (typs, ret) -> SLambda (List.map (check_type cls) typs, check_ret_type cls ret)
    | Class name -> if StringMap.mem name cls then SClass name else raise (Failure ("invalid type: " ^ name))
  and check_ret_type cls = function
      VarType typ -> SVarType (check_type cls typ)
    | BuiltInTyp builtin -> SBuiltInTyp builtin
    | Void -> SVoid
  in
  let check_call lamb args =
    let ret_typ = match lamb with
        SVarType (SLambda (types, ret)), _ ->
        let _ = try List.map2 confirm_type types args
          with Invalid_argument _ -> raise (Failure "lambda expression call: invalid arguments")
        in
        ret
      | SBuiltInTyp builtin, _ ->
        let check_same_type exprs = match exprs with [] -> [] | (r, _) :: _ -> List.map (confirm_ret_type r) exprs in
        (match builtin with
          Add | Mult -> (match check_same_type args with
            SVarType SInt :: _ when List.length args > 1 -> SVarType SInt
          | SVarType SDouble :: _ when List.length args > 1 -> SVarType SDouble
          | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
        | Sub | Div -> (match args with
            [SVarType SInt, _; SVarType SInt, _] -> SVarType SInt
          | [SVarType SDouble, _; SVarType SDouble, _] -> SVarType SDouble
          | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
        | Mod -> (match args with
            [SVarType SInt, _; SVarType SInt, _] -> SVarType SInt
          | _ -> raise (Failure "mod: invalid arguments"))
        | Eq | Neq -> (match args with
            [SVarType SInt, _; SVarType SInt, _]
          | [SVarType SDouble, _; SVarType SDouble, _]
          | [SVarType SBool, _; SVarType SBool, _]
          | [SVarType SString, _; SVarType SString, _] -> SVarType SBool
          | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
        | Lt | Gt | Leq | Geq -> (match args with
            [SVarType SInt, _; SVarType SInt, _]
          | [SVarType SDouble, _; SVarType SDouble, _] -> SVarType SBool
          | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
        | And | Or -> (match check_same_type args with
            SVarType SBool :: _ when List.length args > 1 -> SVarType SBool
          | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
        | Not -> (match args with
            [SVarType SBool, _] -> SVarType SBool
          | _ -> raise (Failure "not: invalid argument"))
        | I2d -> (match args with
            [SVarType SInt, _] -> SVarType SDouble
          | _ -> raise (Failure "i2d: invalid argument"))
        | D2i -> (match args with
            [SVarType SDouble, _] -> SVarType SInt
          | _ -> raise (Failure "d2i: invalid argument"))
        | Cons -> (match args with
            [SVarType t1, _; SVarType (SList t2), _] when t1 = t2 -> SVarType (SList t1)
          | _ -> raise (Failure "cons: invalid arguments"))
        | Car -> (match args with
            [SVarType (SList t), _] -> SVarType t
          | _ -> raise (Failure "car: invalid argument"))
        | Cdr -> (match args with
            [SVarType (SList t), _] -> SVarType (SList t)
          | _ -> raise (Failure "cdr: invalid argument"))
        | Append -> (match check_same_type args with
            SVarType (SList t) :: _ when List.length args > 1 -> SVarType (SList t)
          | _ -> raise (Failure "append: invalid arguments"))
        | Empty -> (match args with
            [SVarType (SList _), _] -> SVarType SBool
          | _ -> raise (Failure "empty: invalid argument"))
        | If -> (match args with
            [SVarType SBool, _; t1, _; t2, _] when t1 = t2 -> t1
          | _ -> raise (Failure "if: invalid arguments"))
        | Begin -> (match List.rev args with
            (t1, _) :: _ -> t1
          | _ -> raise (Failure "begin: expressions expected"))
        | Print -> (match args with
            [SVarType SString, _] | [SVarType SInt, _]
          | [SVarType SDouble, _] | [SVarType SBool, _] -> SVoid
          | _ -> raise (Failure "print: invalid argument"))
        | None -> if List.length args = 0 then SVoid else raise (Failure "void: no arguments allowed"))
      | _ -> raise (Failure "invalid call: not a lambda")
    in
    ret_typ, SCall (lamb, args)
  in
  let rec check_expr syms cls =
    let rec type_of_id id = function
        [] -> raise (Failure ("undeclared identifier: " ^ id))
      | sym :: rest -> try StringMap.find id sym with Not_found -> type_of_id id rest
    in
    function
      Lit l -> SVarType SInt, SLit l
    | DoubleLit l -> SVarType SDouble, SDoubleLit l
    | BoolLit l -> SVarType SBool, SBoolLit l
    | StringLit l -> SVarType SString, SStringLit l
    | BuiltIn builtin -> SBuiltInTyp builtin, SBuiltIn builtin
    | Id name -> SVarType (type_of_id name syms), SId name
    | MemId (first, middle, last) ->
      let get_class_name outer id =
        let enclosing = List.hd (List.rev outer) in
        let vars, _ = StringMap.find enclosing cls in
        let id_type = try StringMap.find id vars
          with Not_found -> raise (Failure ("class " ^ enclosing ^ " does not have member " ^ id))
        in
        match id_type with
            SClass name -> outer @ [name]
          | _ -> raise (Failure ("member " ^ id ^ " is not a class"))
      in
      let first_classname = match type_of_id first syms with
          SClass name -> name
        | _ -> raise (Failure (first ^ " is not a class"))
      in
      let cls_names = List.fold_left get_class_name [first_classname] middle in
      let cls_name = List.hd (List.rev cls_names) in
      let vars, _ = StringMap.find cls_name cls in
      let typ = try StringMap.find last vars
        with Not_found -> raise (Failure ("class " ^ cls_name ^ " does not have member " ^ last))
      in
      SVarType typ, SMemId ((first, first_classname), List.combine middle (List.tl cls_names), last)
    | Call (lamb, args) -> check_call (check_expr syms cls lamb) (List.map (check_expr syms cls) args)
    | Lst (typ, exprs) ->
      let typ = check_type cls typ in
      let exprs' = List.map (check_expr syms cls) exprs in
      let _ = List.map (confirm_type typ) exprs' in
      SVarType (SList typ), SLst (typ, exprs')
    | LambdaExpr (typs, ret, formals, expr) ->
      let typs = List.map (check_type cls) typs in
      let ret = check_ret_type cls ret in
      let sym' = try List.fold_left2 (fun sym typ formal -> if StringMap.mem formal cls
          then raise (Failure (formal ^ " is a class name")) else StringMap.add formal typ sym)
        StringMap.empty typs formals
        with Invalid_argument _ -> raise (Failure "lambda expression: invalid number of formals")
      in
      let expr' = check_expr [sym'; List.hd (List.rev syms)] cls expr in
      SVarType (SLambda (typs, ret)), SLambdaExpr (typs, confirm_ret_type ret expr', formals, expr')
  in
  let check_toplevel (sym, cls, checked) = function
      Bind (typ, name, expr) -> if StringMap.mem name sym
        then raise (Failure ("identifier " ^ name ^ " is already declared"))
        else let typ = check_type cls typ in
          (match typ with
            SLambda _ ->
            let sym = StringMap.add name typ sym in
            let expr' = check_expr [sym] cls expr in
            sym, cls, SBind (confirm_type typ expr', name, expr') :: checked
          | _ ->
            let expr' = check_expr [sym] cls expr in
            StringMap.add name typ sym, cls, SBind (confirm_type typ expr', name, expr') :: checked)
    | DeclClass (name, memlist, constrlst) ->
      let add_member (vars, smembers) (name_mem, typ) =
        if StringMap.mem name_mem vars then raise (Failure ("member " ^ name_mem ^ " is already declared"))
          else let typ = check_type cls typ in StringMap.add name_mem typ vars, (name_mem, typ) :: smembers
      in
      let vars, smembers = List.fold_left add_member (StringMap.empty, []) memlist in
      let check_constructor sym =
        let list_equal l1 l2 = try List.map2 (fun a b ->
            if a <> b then raise (Failure ("class " ^ name ^ ": invalid constructor"))) l1 l2
          with Invalid_argument _ -> raise (Failure ("class " ^ name ^ ": invalid constructor"))
        in
        let mem_names, _ = List.split (StringMap.bindings vars) in
        let _ = list_equal mem_names (List.sort compare constrlst) in
        let typ_list = List.map (fun id -> StringMap.find id vars) constrlst in
        StringMap.add name (SLambda (typ_list, SVarType (SClass name))) sym
      in
      check_constructor sym, StringMap.add name (vars, constrlst) cls, SDeclClass (name, smembers, constrlst) :: checked
    | Expr expr -> sym, cls, SExpr (check_expr [sym] cls expr) :: checked
  in
  let sym, cls, checked = List.fold_left check_toplevel (StringMap.empty, StringMap.empty, []) toplevels in
  sym, cls, List.rev checked
