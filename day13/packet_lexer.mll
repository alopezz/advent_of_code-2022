{
open Packet_parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
   | '\n'        { EOL }
   | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
   | '['  { LBRACKET }
   | ']'  { RBRACKET }
   | ','  { COMMA }
   | eof  { EOF }