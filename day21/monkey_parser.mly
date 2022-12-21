
%token <int> INT
%token <string> NAME
%token ADD MINUS MUL DIV COLON
%token EOF
%start monkey
%type <string * Monkey.monkey_job> monkey

%%
monkey:
    NAME COLON expr  { ($1, $3) }
;

expr:
    NAME ADD NAME { `Sum ($1, $3) }
  | NAME MINUS NAME { `Sub ($1, $3) }
  | NAME MUL NAME { `Mul ($1, $3) }
  | NAME DIV NAME { `Div ($1, $3) }
  | INT { `Value $1 }
;
