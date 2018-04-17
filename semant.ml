open Ast
open Sast

module StringMap = Map.Make(String)

let check toplevels =
  let confirm_type typ (ret, _) =
    if ret = SVarType typ then typ else
      raise (Failure (string_of_sret_typ ret ^ " and " ^ string_of_styp typ ^ " do not match"))
  in
  let confirm_type2 typ (ret, _) =
    if ret = SVarType typ then typ else
      raise (Failure (string_of_sret_typ ret ^ " and " ^ string_of_styp typ ^ " do not match!!!!"))
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
    | Class name -> if StringMap.mem name cls then SClass name
      else raise (Failure ("invalid type: " ^ name))
  and check_ret_type cls = function
      VarType typ -> SVarType (check_type cls typ)
    | BuiltInTyp builtin -> SBuiltInTyp builtin
    | Void -> SVoid
  in
  let rec check_call lamb args = ((match lamb with
      (SVarType (SLambda (types, ret)), _) -> let _ = try List.map2 confirm_type types args
        with Invalid_argument _ -> raise (Failure "lambda expression call: invalid arguments") in
      ret
    | (SBuiltInTyp builtin, _) -> let check_same_type exprs = match exprs with
        [] -> []
      | (r, _) :: _ -> List.map (confirm_ret_type r) exprs
      in
      (match builtin with
        Add | Mult -> (match check_same_type args with
          SVarType SInt :: _ when List.length args > 1 -> SVarType SInt
        | SVarType SDouble :: _ when List.length args > 1 -> SVarType SDouble
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | Sub | Div -> (match args with
          [(SVarType SInt, _); (SVarType SInt, _)] -> SVarType SInt
        | [(SVarType SDouble, _); (SVarType SDouble, _)] -> SVarType SDouble
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | Mod -> (match args with
          [(SVarType SInt, _); (SVarType SInt, _)] -> SVarType SInt
        | _ -> raise (Failure "mod: invalid arguments"))
      | Eq | Neq -> (match args with
          [(SVarType SInt, _); (SVarType SInt, _)]
        | [(SVarType SDouble, _); (SVarType SDouble, _)]
        | [(SVarType SBool, _); (SVarType SBool, _)]
        | [(SVarType SString, _); (SVarType SString, _)] -> SVarType SBool
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | Lt | Gt | Leq | Geq -> (match args with
          [(SVarType SInt, _); (SVarType SInt, _)]
        | [(SVarType SDouble, _); (SVarType SDouble, _)] -> SVarType SBool
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | And | Or -> (match check_same_type args with
          SVarType SBool :: _ when List.length args > 1 -> SVarType SBool
        | _ -> raise (Failure (string_of_built_in builtin ^ ": invalid arguments")))
      | Not -> (match args with
          [(SVarType SBool, _)] -> SVarType SBool
        | _ -> raise (Failure "not: invalid argument"))
      | I2d -> (match args with
          [(SVarType SInt, _)] -> SVarType SDouble
        | _ -> raise (Failure "i2d: invalid argument"))
      | D2i -> (match args with
          [(SVarType SDouble, _)] -> SVarType SInt
        | _ -> raise (Failure "d2i: invalid argument"))
      | Cons -> (match args with
          [(SVarType t1, _); (SVarType (SList t2), _)] when t1 = t2 -> SVarType (SList t1)
        | _ -> raise (Failure "cons: invalid arguments"))
      | Car -> (match args with
          [(SVarType (SList t), _)] -> SVarType t
        | _ -> raise (Failure "car: invalid argument"))
      | Cdr -> (match args with
          [(SVarType (SList t), _)] -> SVarType (SList t)
        | _ -> raise (Failure "cdr: invalid argument"))
      | Append -> (match check_same_type args with
          SVarType (SList t) :: _ when List.length args > 1 -> SVarType (SList t)
        | _ -> raise (Failure "append: invalid arguments"))
      | Empty -> (match args with
          [(SVarType (SList _), _)] -> SVarType SBool
        | _ -> raise (Failure "empty: invalid argument"))
      | If -> (match args with
          [(SVarType SBool, _); (t1, _); (t2, _)] when t1 = t2 -> t1
        | _ -> raise (Failure "if: invalid arguments"))
      | Begin -> (match List.rev args with
          (t1, _) :: _ -> t1
        | _ -> raise (Failure "begin: expressions expected"))
      | Print -> (match args with
          [(SVarType SString, _)] | [(SVarType SInt, _)]
        | [(SVarType SDouble, _)] | [(SVarType SBool, _)] -> SVoid
        | _ -> raise (Failure "print: invalid argument")))
    | _ -> raise (Failure "invalid call: not a lambda")), SCall (lamb, args))
  and check_expr syms cls =
    let rec type_of_id id = function
        [] -> raise (Failure ("undeclared identifier: " ^ id))
      | sym :: rest -> try StringMap.find id sym with Not_found -> type_of_id id rest
    in
  function
    Lit l -> (SVarType SInt, SLit l)
  | DoubleLit l -> (SVarType SDouble, SDoubleLit l)
  | BoolLit l -> (SVarType SBool, SBoolLit l)
  | StringLit l -> (SVarType SString, SStringLit l)
  | BuiltIn builtin -> (SBuiltInTyp builtin, SBuiltIn builtin)
  | Id name -> (SVarType (type_of_id name syms), SId name)
  | MemId (names, name) -> (SVarType (
    let global = List.hd (List.rev syms) in
    let rec get_combined_cls_name outer_name = (function
        [] -> outer_name
      | hd :: tl -> let (vars, _) = try StringMap.find outer_name cls with Not_found ->
          raise (Failure (outer_name ^ " is not a class"))
        in
        let hd_type = try StringMap.find hd vars with Not_found ->
          raise (Failure ("class " ^ outer_name ^ " does not have non-static member " ^ hd))
        in
        (match hd_type with
            SClass name -> get_combined_cls_name name tl
          | _ -> raise (Failure ("member " ^ hd ^ " is not a class"))))
    in
    (match names with
        [id] when StringMap.mem id cls -> (try StringMap.find (id ^ "." ^ name) global
          with Not_found -> raise (Failure ("class " ^ id ^ " does not have static member " ^ name)))
      | _ -> let cls_name = (match names with
          | id1 :: (id2 :: tl) when StringMap.mem id1 cls -> let class_id = id1 ^ "." ^ id2 in
            let class_name = (match (try StringMap.find class_id global with Not_found ->
              raise (Failure ("class " ^ id1 ^ " does not have static member " ^ id2))) with
                SClass name -> name
              | _ -> raise (Failure (class_id ^ " is not a class")))
            in
            get_combined_cls_name class_name tl
          | id :: tl -> get_combined_cls_name
            (match (try type_of_id id syms with Not_found ->
              raise (Failure ("undeclared identifier: " ^ id))) with
                SClass name -> name
              | _ -> raise (Failure (id ^ " is not a class"))) tl
          | _ -> raise (Failure "internal error"))
        in
        let (vars, _) = StringMap.find cls_name cls in
        try StringMap.find name vars with Not_found ->
          raise (Failure ("class " ^ cls_name ^ " does not have non-static member " ^ name)))),
    SMemId (names, name))
  | Call (lamb, args) -> check_call (check_expr syms cls lamb) (List.map (check_expr syms cls) args)
  | Lst (typ, exprs) -> let typ = check_type cls typ in
      let exprs' = List.map (check_expr syms cls) exprs in
      let _ = List.map (confirm_type typ) exprs' in
      (SVarType (SList typ), SLst (typ, exprs'))
  | LambdaExpr (typs, ret, formals, expr) -> let typs = List.map (check_type cls) typs in
    let ret = check_ret_type cls ret in (SVarType (SLambda (typs, ret)),
      let sym' = (try List.fold_left2 (fun sym typ formal -> if StringMap.mem formal cls
        then raise (Failure (formal ^ " is a class name")) else StringMap.add formal typ sym)
        StringMap.empty typs formals
        with Invalid_argument _ -> raise (Failure "lambda expression: invalid number of formals"))
      in
      let expr' = check_expr (sym' :: syms) cls expr in
      SLambdaExpr (typs, confirm_ret_type ret expr', formals, expr'))
  in
  let check_toplevel (sym, cls, checked) = function
      Bind (typ, name, expr) -> if StringMap.mem name sym
      then raise (Failure ("identifier " ^ name ^ " is already declared"))
      else let typ = check_type cls typ in (match typ with
          SLambda (_, _) -> let sym = StringMap.add name typ sym in (sym, cls,
            let expr' = check_expr [sym] cls expr in SBind (confirm_type2 typ expr', name, expr') :: checked)
        | _ -> (StringMap.add name typ sym, cls,
          let expr' = check_expr [sym] cls expr in SBind (confirm_type typ expr', name, expr') :: checked))
    | DeclClass (name, memlist, constrlist) ->
      let add_members (sym, vars, smembers) = function
          MemConst (name_mem, typ, expr) -> let typ = check_type cls typ in
          let mem_id = name ^ "." ^ name_mem in
          if StringMap.mem mem_id sym then raise (Failure ("identifier " ^ mem_id ^ " is already declared"))
          else (match typ with
              SLambda (_, _) -> let sym' = StringMap.add mem_id typ sym in
              let expr' = check_expr [sym'] cls expr in
              (sym', vars, SMemConst (name_mem, confirm_type typ expr', expr') :: smembers)
            | _ -> let expr' = check_expr [sym] cls expr in
              (StringMap.add mem_id typ sym, vars,
                SMemConst (name_mem, confirm_type typ expr', expr') :: smembers))
        | MemVar (name_mem, typ) -> if StringMap.mem name_mem vars
          then raise (Failure ("member " ^ name_mem ^ " is already declared"))
          else let typ = check_type cls typ in
            (sym, StringMap.add name_mem typ vars, SMemVar (name_mem, typ) :: smembers)
      in
      let (sym', vars, smembers) = List.fold_left add_members (sym, StringMap.empty, []) memlist in
      let check_constructor sym =
        let list_equal l1 l2 = try List.map2 (fun a b ->
          if a <> b then raise (Failure ("class " ^ name ^ ": invalid constructor"))) l1 l2
          with Invalid_argument _ -> raise (Failure ("class " ^ name ^ ": invalid constructor")) in
        let (mem_names, _) = List.split (StringMap.bindings vars) in
        let _ = list_equal mem_names (List.sort compare constrlist) in
        let typ_list = List.map (fun id -> StringMap.find id vars) constrlist in
        StringMap.add name (SLambda (typ_list, SVarType (SClass name))) sym
      in
      (check_constructor sym', StringMap.add name (vars, constrlist) cls,
        SDeclClass (name, smembers, constrlist) :: checked)
    | Expr expr -> (sym, cls, SExpr (check_expr [sym] cls expr) :: checked)
  in
  let (sym, cls, checked) = List.fold_left check_toplevel (StringMap.empty, StringMap.empty, []) toplevels in
  (sym, cls, List.rev checked)
