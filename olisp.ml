let string_of_typ = function
	Int -> "int"
  | Bool -> "bool"
  | Double -> "double"
  | String -> "string"
  | List -> "list"
  | Class -> "Class"

let () = 
 let usage_msg = "usage: ./olisp.native [-a|-s|-l|-c] [file.mc]" in
  let channel = ref stdin in
  Arg.parse [] (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in  
  let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ";\n" in 
  let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n" in
  let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) in
  print_string (Ast.string_of_program ast)