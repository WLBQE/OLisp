
%{
open Ast
%}

%token LPAREN RPAREN LBRACK RBRACK PLUS MINUS TIMES DIVIDE MODULE VOID
%token NOT EQ NEQ LT LEQ GT GEQ AND OR LST CONS CAR CDR
%token IF BEGAN
%token DEFINE LAMBDA CLASS MEMBER CONSTR ARROW
%token <int> INT
%token <bool> BOOL
%token <string> STRING DOUBLE ID
%token EOF

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

builtin:
  | PLUS             { Add    }
  | MINUS            { Sub    }
  | TIMES            { Mult   }
  | DIVIDE           { Div    }
  | MODULE           { Mod    }
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

cls:
  LPAREN CLASS ID mem_list constructor RPAREN { ($3, List.rev $4, $5) }

typ_list:
    typ { [$1] }
  | typ_list typ { $2 :: $1 }

typ:
    var  { Var($1) }
  | cls  { Class($1) }
  | VOID { Void }

defvar:
  LPAREN DEFINE LPAREN typ ID RPAREN expr RPAREN { ($4, $5, $7) }

defmember:
  mem { $1 }

defmember_list:
    defmember { [$1] }
  | defmember_list defmember { $2 :: $1 }

formal_list_par:
  LPAREN formal_list RPAREN { List.rev $2 }

formal_list:
    ID { [$1] }
  | formal_list ID { $2 :: $1 }

constructor:
  LPAREN CONSTR formal_list_par RPAREN { $3 }

defconstructor:
  LPAREN CONSTR formal_list_par RPAREN { $3 }

defclass:
  LPAREN CLASS ID defmember_list defconstructor RPAREN { ($3, List.rev $4, $5) }

def:
    defvar    { DefVar($1) }
  | defclass  { DefClass($1) }

expr_list_par:
  LPAREN expr_list RPAREN { List.rev $2 }

expr_list:
    expr { [$1] }
  | expr_list expr { $2 :: $1 }

expr:
    INT                  { Lit($1)                }
  | DOUBLE	             { DoubleLit($1)          }
  | BOOL                 { BoolLit($1)            }
  | STRING               { StringLit($1)          }
  | builtin              { Builtin($1)            }
  | ID                   { Id($1)                 }
  | BEGAN expr_list_par  { Began($2)              }
  | expr expr_list_par   { Call($1, $2)           }
  | LPAREN LAMBDA typ formal_list_par expr
                         { LambdaExpr($3, $4, $5) }
  | LPAREN DEFINE def RPAREN
                         { Define($3)             }
