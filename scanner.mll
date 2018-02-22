{ open Parser
  type status = { mutable is_new: bool }
  let s = { is_new = true }
  let check status =
    if status.is_new then () else raise (Failure("Syntax error"));
    status.is_new <- false
  let reset status = status.is_new <- true }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
    [' ' '\t' '\r' '\n'] { reset s; token lexbuf }
  | "//"          { reset s; comment lexbuf }
  | '('           { reset s; LPAREN }
  | ')'           { reset s; RPAREN }
  | '['           { reset s; LBRACK }
  | ']'           { reset s; RBRACK }
  | "int"         { check s; INT }
  | "double"      { check s; DOUBLE }
  | "bool"        { check s; BOOL }
  | "string"      { check s; STRING }
  | "void"        { check s; VOID }
  | '+'           { check s; PLUS }
  | '-'           { check s; MINUS }
  | '*'           { check s; TIMES }
  | '/'           { check s; DIVIDE }
  | '%'           { check s; MODULO }
  | "and"         { check s; AND }
  | "or"          { check s; OR }
  | "not"         { check s; NOT }
  | '='           { check s; EQ }
  | "!="          { check s; NEQ }
  | '<'           { check s; LT }
  | '>'           { check s; GT }
  | "<="          { check s; LEQ }
  | ">="          { check s; GEQ }
  | "list"        { check s; LIST }
  | "cons"        { check s; CONS }
  | "car"         { check s; CAR }
  | "cdr"         { check s; CDR }
  | "append"      { check s; APPEND }
  | "empty"       { check s; EMPTY }
  | "if"          { check s; IF }
  | "begin"       { check s; BEGIN }
  | "print"       { check s; PRINT }
  | "define"      { check s; DEFINE }
  | "lambda"      { check s; LAMBDA }
  | "->"          { check s; ARROW }
  | "class"       { check s; CLASS }
  | "member"      { check s; MEMBER }
  | "constructor" { check s; CONSTR }
  | "true"        { check s; BOOLLIT(true) }
  | "false"       { check s; BOOLLIT(false) }
  | digit+ as lxm { check s; LIT(int_of_string lxm) }
  | letter (letter | digit | '_')* as lxm { check s; ID(lxm) }
  | letter (letter | digit | '_')* '.' letter (letter | digit | '_')* as lxm { check s; MEMID(lxm) }
  | digit+ '.' digit+ as lxm { check s; DOUBLELIT(lxm) }
  | '"'           { check s; string_lit (Buffer.create 16) lexbuf }
  | eof           { EOF }
  | _ as char     { raise (Failure("Unexpected character: " ^ Char.escaped char)) }

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
  | [^ '"' '\\']+  as lxm { Buffer.add_string buf lxm; string_lit buf lexbuf }
  | _ as char     { raise (Failure("Illegal string character: " ^ Char.escaped char)) }
  | eof           { raise (Failure("String is not terminated")) }
