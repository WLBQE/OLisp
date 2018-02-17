(* { open Parser } *)

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
      [' ' '\t' '\r' '\n']		{ token lexbuf }
    | "//" 	     	   		{ comment lexbuf }
    | "->"		   		{ POINT }
    | "if"		   		{ IF }
    | "and"		   		{ AND }
    | "or"		   		{ OR }
    | "not"		   		{ NOT }
    | "define"		   		{ DEFINE }
    | "int"		   		{ INT }
    | "double"		   		{ DOUBLE }
    | "bool"		   		{ BOOL }
    | "string"		   		{ STRING }
    | "list"		   		{ LIST }
    | "cons"		   		{ CONS }
    | "car"		   		{ CAR }
    | "cdr"		   		{ CDR }
    | "append"		   		{ APPEND }
    | "empty"		   		{ EMPTY }
    | "began"                             { BEGAN }
    | "lambda"                            { LAMBDA }
    | '('		   		{ LPAREN }
    | ')'		   		{ RPAREN }
    | '['				{ LBRACK }
    | ']'				{ RBRACK }
    | '+'                  		{ PLUS }
    | '-'                  		{ MINUS }
    | '*'                  		{ TIMES }
    | '/'                  		{ DIVIDE }
    | '%'		   		{ MODULE }
    | '='                  		{ EQ }
    | "!="		   		{ NEQ }
    | '<'		   		{ LT }
    | "<="		   		{ LEQ }
    | '>'		   		{ GT }
    | ">="		   		{ GEQ }
    | eof				{ EOF }
    | digit+ as lxm	   		{ LIT(int_of_string lxm) }
    | letter (letter | digit | '_')*    { ID(var) }
    | digit+ '.' digit* as lxm		{ DOUBLELIT(lxm) }
    | '\"' [^ '\"']+ '\"' as lxm	{ STRINGLIT(lxm) }
    | _ as char	     		   	{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
      '\n'		   { token lexbuf }
    | _			   { comment lexbuf }
