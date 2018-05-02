%{ open Ast %}

%token LPAREN RPAREN LBRACK RBRACK
%token INT DOUBLE BOOL STRING VOID
%token PLUS MINUS TIMES DIVIDE MODULO
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%token I2D D2I
%token LIST CONS CAR CDR APPEND EMPTY
%token IF BEGIN PRINT
%token DEFINE LAMBDA ARROW
%token CLASS MEMBER CONSTR DOT
%token <int> LIT
%token <bool> BOOLLIT
%token <string> ID MEMID STRINGLIT DOUBLELIT
%token EOF

%start program
%type <Ast.program> program

%%

program:
    top_level_list EOF { List.rev $1 }

top_level_list:
    /* nothing */            { [] }
  | top_level_list top_level { $2 :: $1 }

top_level:
    LPAREN DEFINE LPAREN typ ID RPAREN expr RPAREN { Bind ($4, $5, $7) }
  | LPAREN CLASS ID mem_list LPAREN CONSTR formal_list RPAREN RPAREN
    { DeclClass ($3, List.rev $4, List.rev $7) }
  | expr { Expr($1) }

typ:
    INT    { Int }
  | DOUBLE { Double }
  | BOOL   { Bool }
  | STRING { String }
  | ID     { Class $1 }
  | LBRACK typ RBRACK { List $2 }
  | LPAREN type_list ARROW ret_type RPAREN { Lambda ($2, $4) }

type_list:
    VOID          { [] }
  | var_type_list { List.rev $1 }

var_type_list:
    typ               { [$1] }
  | var_type_list typ { $2 :: $1 }

ret_type:
    typ  { VarType $1 }
  | VOID { Void }

expr:
    LIT       { Lit $1 }
  | DOUBLELIT { DoubleLit $1 }
  | BOOLLIT   { BoolLit $1 }
  | STRINGLIT { StringLit $1 }
  | built_in  { BuiltIn $1 }
  | ID        { Id $1 }
  | MEMID     { Ast.split_mem_id $1 }
  | LPAREN expr expr_list RPAREN { Call ($2, List.rev $3) }
  | LPAREN LIST typ expr_list RPAREN { Lst ($3, List.rev $4) }
  | LPAREN LAMBDA LPAREN type_list ARROW ret_type RPAREN LPAREN formal_list RPAREN expr RPAREN
    { LambdaExpr ($4, $6, List.rev $9, $11) }

built_in:
    PLUS   { Add }
  | MINUS  { Sub }
  | TIMES  { Mult }
  | DIVIDE { Div }
  | MODULO { Mod }
  | EQ     { Eq }
  | NEQ    { Neq }
  | LT     { Lt }
  | GT     { Gt }
  | LEQ    { Leq }
  | GEQ    { Geq }
  | AND    { And }
  | OR     { Or }
  | NOT    { Not }
  | I2D    { I2d }
  | D2I    { D2i }
  | CONS   { Cons }
  | CAR    { Car }
  | CDR    { Cdr }
  | APPEND { Append }
  | EMPTY  { Empty }
  | IF     { If }
  | BEGIN  { Begin }
  | PRINT  { Print }

expr_list:
    /* nothing */  { [] }
  | expr_list expr { $2 :: $1 }

formal_list:
    /* nothing */  { [] }
  | formal_list ID { $2 :: $1 }

mem_list:
    mem          { [$1] }
  | mem_list mem { $2 :: $1 }

mem:
    LPAREN MEMBER LPAREN typ ID RPAREN RPAREN { ($5, $4) }
