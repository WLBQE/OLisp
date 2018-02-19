{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
      [' ' '\t' '\r' '\n']		{ token lexbuf }
    | "//" 	   	   		{ comment lexbuf }
    | "->"		   		{ ARROW }
    | "if"		   		{ IF }
    | "and"		   		{ AND }
    | "or"              { OR }
    | "not"		   		{ NOT }
    | "define"		   		{ DEFINE }
    | "int"		   		{ INT }
    | "double"		   		{ DOUBLE }
    | "bool"		  		{ BOOL }
    | "string"		   		{ STRING }
    | "void"            		{ VOID }
    | "list"		   		{ LST }
    | "cons"		   		{ CONS }
    | "car"		   		{ CAR }
    | "cdr"		   		{ CDR }
    | "append"		   		{ APPEND }
    | "empty"		   		{ EMPTY }
    | "begin"           		{ BEGIN }
    | "lambda"          		{ LAMBDA }
    | "class"           		{ CLASS }
    | "member"          		{ MEMBER }
    | "constructor"     		{ CONSTR }
    | "true"				{ BLIT(true) }
    | "false"				{ BLIT(false) }
    | '('		   		{ LPAREN }
    | ')'		   		{ RPAREN }
    | '['				{ LBRACK }
    | ']'				{ RBRACK }
    | '+'               		{ PLUS }
    | '-'               		{ MINUS }
    | '*'               		{ TIMES }
    | '/'               		{ DIVIDE }
    | '%'		   		{ MODULO }
    | '='               		{ EQ }
    | "!="		   		{ NEQ }
    | '<'		   		{ LT }
    | "<="		   		{ LEQ }
    | '>'		   		{ GT }
    | ">="		   		{ GEQ }
    | eof				{ EOF }
    | digit+ as lxm	   		{ LIT(int_of_string lxm) }
    | letter (letter | digit | '_')* as var { ID(var) }
    | digit+ '.' digit* as lxm		{ DOUBLELIT(lxm) }
    | '\"' [^ '\"']+ '\"' as lxm	{ STRINGLIT(lxm) }
    | _ as char	     		   	{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
      '\n'		   { token lexbuf }
    | _			   { comment lexbuf }
