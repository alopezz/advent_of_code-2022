{
open Monkey_parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
     ' '     { token lexbuf }     (* skip blanks *)
   | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
   | ['a'-'z']+ as lxm { NAME(lxm) }
   | '+'  { ADD }
   | '-'  { MINUS }
   | '*'  { MUL }
   | '/'  { DIV }
   | ':'  { COLON }
   | eof | '\n'  { EOF }