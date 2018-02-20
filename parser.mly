
%{
open Ast
%}

%token LPAREN RPAREN LBRACK RBRACK PLUS MINUS TIMES DIVIDE MODULO VOID
%token INT DOUBLE BOOL STRING
%token NOT EQ NEQ LT LEQ GT GEQ AND OR LST CONS CAR CDR APPEND EMPTY
%token IF BEGIN
%token DEFINE LAMBDA CLASS MEMBER CONSTR ARROW
%token <int> LIT
%token <bool> BLIT
%token <string> STRINGLIT DOUBLELIT ID
%token EOF

%start program
%type <Ast.program> program

%%

program:
  expr EOF { $1 }

builtin:
    PLUS             { Add    }
  | MINUS            { Sub    }
  | TIMES            { Mult   }
  | DIVIDE           { Div    }
  | MODULO           { Mod    }
  | EQ               { Eq     }
  | NEQ              { Neq    }
  | LT               { Lt     }
  | GT               { Gt     }
  | LEQ              { Leq    }
  | GEQ              { Geq    }
  | AND              { And    }
  | OR               { Or     }
  | NOT              { Not    }
  | LST              { Lst    }
  | CONS             { Cons   }
  | CAR              { Car    }
  | CDR              { Cdr    }
  | APPEND           { Append }
  | EMPTY            { Empty  }
  | IF               { If     }

var:
    INT               { Int }
  | CLASS             { Class }
  | DOUBLE            { Double }
  | BOOL              { Bool }
  | STRING            { String }
  | LBRACK typ RBRACK { List($2) }
  | LPAREN typ_list ARROW typ RPAREN
                      { Lambda(List.rev $2, $4) }

mem_list:
    mem { [$1] }
  | mem_list mem { $2 :: $1 }

mem:
    LPAREN MEMBER mem_var RPAREN { $3 }
  | LPAREN MEMBER mem_const RPAREN { $3 }

mem_var:
  LPAREN typ ID RPAREN { MemVar($3, $2) }

mem_const:
  LPAREN typ ID expr RPAREN { MemConst($3, $2, $4) }

typ_list:
    typ { [$1] }
  | typ_list typ { $2 :: $1 }

typ:
    var  { Var($1) }
  | VOID { Void }

defvar:
  LPAREN DEFINE LPAREN var ID RPAREN expr RPAREN { ($4, $5, $7) }

formal_list_par:
  LPAREN formal_list RPAREN { List.rev $2 }

formal_list:
    ID { [$1] }
  | formal_list ID { $2 :: $1 }

constructor:
  LPAREN CONSTR formal_list_par RPAREN { $3 }

defclass:
  LPAREN CLASS ID mem_list constructor RPAREN { ($3, List.rev $4, $5) }

def:
    defvar    { DefVar($1) }
  | defclass  { DefClass($1) }

expr_list:
         { [] }
  | expr_list expr { $2 :: $1 }

expr:
    LIT                     { Lit($1)                }
  | DOUBLELIT	              { DoubleLit($1)          }
  | BLIT                 { BoolLit($1)            }
  | STRINGLIT            { StringLit($1)          }
  | builtin              { Builtin($1)            }
  | ID                   { Id($1)                 }
  | LPAREN BEGIN expr_list RPAREN  { Begin($3)              }
  | LPAREN LAMBDA LPAREN typ_list ARROW typ RPAREN formal_list_par expr RPAREN
                         { LambdaExpr($4, $6, $8, $9) }
  | LPAREN DEFINE def RPAREN
                         { Define($3)             }
  | LPAREN expr expr_list RPAREN  { Call($2, $3)           }
