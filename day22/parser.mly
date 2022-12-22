%{
    open Parser_types
%}

%token <int> INT
%token LEFT RIGHT
%token SPACE WALL_CHAR OPEN_CHAR
%token EOL EOF
%start notes
%type <Parser_types.map * Parser_types.path> notes

%%
notes:
    map path { (Array.of_list $1, $2) }
;

map:
    map_row map { (Array.of_list $1) :: $2 }
  | map_row EOL { [Array.of_list $1] }
;

map_row:
    map_tile map_row { $1 :: $2 }
  | map_tile EOL { [$1] }
;

map_tile:
    SPACE  { Empty }
  | WALL_CHAR  { Wall }
  | OPEN_CHAR  { Open }
;

path:
    instruction path { $1 :: $2 }
  | instruction EOL  { [$1] }
;

instruction:
    INT   { Move $1 }
  | LEFT  { TurnLeft }
  | RIGHT { TurnRight}
;
