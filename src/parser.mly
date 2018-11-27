%token <int> NUM
%token <string> IDENT

%token TRUE FALSE
%token ASSIGN IF THEN ELSE WHILE DO REPEAT UNTIL FOR TO
%token LPAREN RPAREN
%token SEMI
%token NOT AND OR EQ NE LE GE LT GT
%token PLUS MINUS TIMES
%token EOF

%left AND
%left OR
%left EQ NE LE GE LT GT
%left PLUS MINUS
%left TIMES

%nonassoc NOT

%start program
%type <Language.stm> program

%%

a_expr:
    NUM                  { Language.Num($1) }
  | IDENT                { Language.Var($1) }
  | a_expr PLUS a_expr   { Language.Sum($1, $3) }
  | a_expr MINUS a_expr  { Language.Sub($1, $3) }
  | a_expr TIMES a_expr  { Language.Mul($1, $3) }
  | LPAREN a_expr RPAREN { $2 }
  ;

b_expr:
    TRUE                 { Language.Bool(true) }
  | FALSE                { Language.Bool(false) }
  | NOT b_expr           { Language.Not($2) }
  | b_expr AND b_expr    { Language.And($1, $3) }
  | b_expr OR b_expr     { Language.Or($1, $3) }
  | a_expr EQ a_expr     { Language.Eq($1, $3) }
  | a_expr NE a_expr     { Language.Ne($1, $3) }
  | a_expr LE a_expr     { Language.Le($1, $3) }
  | a_expr GE a_expr     { Language.Ge($1, $3) }
  | a_expr LT a_expr     { Language.Lt($1, $3) }
  | a_expr GT a_expr     { Language.Gt($1, $3) }
  | LPAREN b_expr RPAREN { $2 }
  ;

stm:
    SEMI                         { Language.Skip }
  | IDENT ASSIGN a_expr SEMI     { Language.Assign($1, $3) }
  | IF b_expr THEN stm ELSE stm  { Language.If($2, $4, $6) }
  | WHILE b_expr DO stm          { Language.While($2, $4) }
  | REPEAT stm UNTIL b_expr SEMI { Language.Repeat($4, $2) }
  | FOR IDENT ASSIGN a_expr
    TO a_expr DO stm             { Language.For($2, $4, $6, $8) }
  | LPAREN stm_list RPAREN       { $2 }
  ;

stm_list:
    stm          { $1 }
  | stm_list stm { Language.Comp($1, $2) }
  ;

program:
    EOF          { Language.Skip }
  | stm_list EOF { $1 }
  ;
