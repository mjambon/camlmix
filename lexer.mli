val get :
  Lexing.lexbuf ->
  [ `Code of string | `Location of int * int | `Text of string ] list
