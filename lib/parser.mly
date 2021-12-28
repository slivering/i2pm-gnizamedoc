%token <int> INT
%token <string> WORD
%token LPAREN RPAREN
%token EOF

%start main             /* the entry point */
%type <Langage.expression> main
%type <Langage.expression list> arguments

%%


main:
    expr = expression; EOF                    { expr }
    ;
expression:
    | i = INT
        { Langage.Constante i }
    
    | v = WORD
        { Langage.Variable v }
    
    | LPAREN; fonc = WORD; args = arguments; RPAREN
        { Langage.Fonction (fonc, args) }
    ;
  
arguments:
    | arg = expression
        { [arg] }
    | arg1 = expression; args = arguments
        { arg1 :: args }
