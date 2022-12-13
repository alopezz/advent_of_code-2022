%{
    open Packet
%}

%token <int> INT
%token LBRACKET RBRACKET
%token COMMA
%token EOL
%token EOF
%start packet_pair_list
%type <Packet.packet_pair list> packet_pair_list
%start packet_list
%type <Packet.packet list> packet_list

%%
packet_pair_list:
    packet_pair EOL packet_pair_list  { $1 :: $3 }
  | packet_pair EOF  { [$1] }
;

packet_list:
    packet packet_list  { $1 :: $2 }
  | packet EOL packet_list  { $1 :: $3 }
  | packet EOF { [$1] }
;

packet_pair:
    packet packet { ($1, $2) }
;

packet:
    list EOL { $1 }
;

list:
    LBRACKET elements RBRACKET  { $2 }
  | LBRACKET RBRACKET { [] }
;

elements:
    value COMMA elements  { $1 :: $3 }
  | value                 { [$1] }

value:
    list    { PList $1 }
  | INT     { PInt $1 }
