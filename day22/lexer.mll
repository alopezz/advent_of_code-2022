{
open Parser        (* The type token is defined in parser.mli *)
}
rule token = parse
     ' '     { SPACE }
   | '#'     { WALL_CHAR }
   | '.'     { OPEN_CHAR }
   | '\n'    { EOL }
   | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
   | 'L'  { LEFT }
   | 'R'  { RIGHT }
   | eof { EOF }