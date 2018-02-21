{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
  | "//"          { comment lexbuf }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '['           { LBRACK }
  | ']'           { RBRACK }
  | "int"         { INT }
  | "double"      { DOUBLE }
  | "bool"        { BOOL }
  | "string"      { STRING }
  | "void"        { VOID }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { TIMES }
  | '/'           { DIVIDE }
  | '%'           { MODULO }
  | "and"         { AND }
  | "or"          { OR }
  | "not"         { NOT }
  | '='           { EQ }
  | "!="          { NEQ }
  | '<'           { LT }
  | '>'           { GT }
  | "<="          { LEQ }
  | ">="          { GEQ }
  | "list"        { LIST }
  | "cons"        { CONS }
  | "car"         { CAR }
  | "cdr"         { CDR }
  | "append"      { APPEND }
  | "empty"       { EMPTY }
  | "if"          { IF }
  | "begin"       { BEGIN }
  | "define"      { DEFINE }
  | "lambda"      { LAMBDA }
  | "->"          { ARROW }
  | "class"       { CLASS }
  | "member"      { MEMBER }
  | "constructor" { CONSTR }
  | "true"        { BOOLLIT(true) }
  | "false"       { BOOLLIT(false) }
  | digit+ as lxm { LIT(int_of_string lxm) }
  | letter (letter | digit | '_')* as lxm { ID(lxm) }
  | digit+ '.' digit+ as lxm { DOUBLELIT(lxm) }
  | '\"' [^'\"']+ '\"' as lxm { STRINGLIT(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("Illegal character: " ^ Char.escaped char)) }

and comment = parse
    '\n' { token lexbuf }
  | _    { comment lexbuf }
