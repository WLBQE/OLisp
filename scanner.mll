{ open Parser
  exception SyntaxError of string }

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
  | "print"       { PRINT }
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
  | '"' { string_lit (Buffer.create 16) lexbuf }
  | eof { EOF }
  | _   { raise (SyntaxError("Unexpected character: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
    '\n' { token lexbuf }
  | _    { comment lexbuf }

and string_lit buf = parse
    '"'           { STRINGLIT(Buffer.contents buf) }
  | "\\\\"        { Buffer.add_char buf '\\'; string_lit buf lexbuf }
  | "\\n"         { Buffer.add_char buf '\n'; string_lit buf lexbuf }
  | "\\r"         { Buffer.add_char buf '\r'; string_lit buf lexbuf }
  | "\\t"         { Buffer.add_char buf '\t'; string_lit buf lexbuf }
  | "\\\""        { Buffer.add_char buf '\"'; string_lit buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); string_lit buf lexbuf }
  | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("String is not terminated")) }
