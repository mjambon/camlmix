%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> IDENT
%token SEP INCLUDE SKIP
%token EOF
%start directive_list
%type <[`Include of string | `Skip] list> directive_list
%%
directive_list:
| directive SEP directive_list   { $1 :: $3 }
| directive EOF                  { [$1] }
| EOF                            { [] }

directive:
| INCLUDE STRING  { `Include $2 }
| SKIP            { `Skip }
