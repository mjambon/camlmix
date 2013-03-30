open Printf
open Genlex
open Parser_directive

let lexer = make_lexer [ ";"; "include"; "skip" ]

let get_token stream =
  fun _ ->
    try 
      match Stream.next stream with
	  String s -> STRING s
	| Int i -> INT i
	| Float f -> FLOAT f
	| Char c -> CHAR c
	| Ident s -> IDENT s
	| Kwd ";" -> SEP
	| Kwd "include" -> INCLUDE
	| Kwd "skip" -> SKIP
	| Kwd _ -> invalid_arg "Directive.get_token"
    with _ -> EOF
      
(* Adapted from String.index *)

let rec index_rec s lim i c =
  if i >= lim then raise Not_found else
    let cur = s.[i] in
  if cur = c then
    i 
  else if cur = ' ' || cur = '\n' || cur = '\t' || cur = '\r' then 
    index_rec s lim (i+1) c
  else 
    raise Not_found

let index s c = index_rec s (String.length s) 0 c

(* *)

exception Forbidden_file of string

let digest_files parents directives =
  List.map
    (function 
	 `Include file -> 
	   let digest = lazy (Digest.file file) in
	   List.iter
	     (fun (pfile, pdigest) -> 
		if Lazy.force digest = Lazy.force pdigest then
		  raise (Forbidden_file file))
	     parents;

	   `Include (file, digest)
	     
       | `Skip -> `Skip)
    directives


let find_directives parents (source, line, char) s =
  try 
    let pos = index s '@' + 1 in
    let code = String.sub s pos (String.length s - pos) in
    let char_stream = Stream.of_string code in
    let token_stream = lexer char_stream in
    let parsing_result = directive_list
			   (get_token token_stream) 
			   (Lexing.from_string "") in
    let result = digest_files parents parsing_result in
    Some result
  with
      Not_found -> None
    | e ->
	eprintf "File %S, line %i, characters %i-%i:\nBad directives\n"
	  source line (char - 1) (char - 1 + String.length s);
	flush stderr;
	raise e

let find_expr (source, line, char) s =
  try 
    let pos = index s '=' + 1 in
    Some (String.sub s pos (String.length s - pos), pos)
  with Not_found -> None
