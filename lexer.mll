{
  (* Alternate line tracking mechanism using Ocaml 3.08 Lexing
     (not used yet for compatibility with older versions of OCaml)

  (* keep track of file position *)
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
              lexbuf.Lexing.lex_curr_p <-
                  { pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                    Lexing.pos_bol = pos.Lexing.pos_cnum }

  (* return string line(char) of file position *)
  let pos_to_string lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    (string_of_int pos.Lexing.pos_lnum)
    ^"."^(string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

  (* return file position *)
  let get_location () =
    let pos = lexbuf.Lexing.lex_curr_p in
    `Location ( pos.Lexing.pos_lnum,
               (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
  *)

  let buf         = Buffer.create     1024
  let add_string  = Buffer.add_string buf
  let add_char    = Buffer.add_char   buf
  let contents () = Buffer.contents   buf
  let clear ()    = Buffer.clear      buf
  let flush ()    = 
    let s = contents () in
    clear ();
    s

  (* the position in the current file *)
  let line = ref 1 (* line numbered from 1 *)
  let char = ref 1 (* character in the current line numbered from 1 *)

  let newfile () =
    line := 1;
    char := 1
  let location () = `Location (!line, !char)
  let newline  () =
    add_char '\n';
    incr line;
    char := 1
  let newchars n = char := !char + n
  let init () = 
    clear   ();
    newfile ()
  let add s =
    add_string s;
    newchars (String.length s)
  let addc c =
    add_char c;
    newchars 1
  let length lexbuf =
    Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf
}
let nl                 = "\r\n" | "\n" | "\r"
let text_ignore_ws_in  = [ ' ' '\t' ]* "##."
let code_ignore_ws_out = ".##" [ ' ' '\t' ]*
let code_ignore_wsnl_out = ".##" [ ' ' '\t' ]* nl

rule text = parse
| "###" "#"+ as str { let len = String.length str - 1 in
                      add (String.sub str 0 len); newchars len; text lexbuf }
| "###" { add "##"; newchars 1; text lexbuf }

| text_ignore_ws_in
| "##"  { newchars (length lexbuf);
	  let t   = `Text (flush ()) in
	  let loc = location () in
	  t :: loc :: code lexbuf }
| nl     { newline (); text lexbuf }
| eof    { [`Text (flush ())] }
| _      { addc (Lexing.lexeme_char lexbuf 0); text lexbuf }
and code = parse
| "."? "###" "#"+ as str 
              { let len = String.length str - 1 in
		add (String.sub str 0 len); 
		newchars 1;
		code lexbuf }
| "."? "###" as str   
              { let len = String.length str - 1 in
		add (String.sub str 0 len); 
		newchars 1;
		code lexbuf }
| code_ignore_ws_out
         { newchars (length lexbuf);
	   let e   = `Code (flush ()) in
	   let loc = location () in
           e :: loc :: text lexbuf }
| code_ignore_wsnl_out
         { newchars (length lexbuf);
	   newline ();
	   let e   = `Code (flush ()) in
	   let loc = location () in
           e :: loc :: text lexbuf }
| "##"   { newchars 2;
	   let e   = `Code (flush ()) in
	   let loc = location () in
	   e :: loc :: text lexbuf }
| nl     { newline (); code lexbuf }
| eof    { [`Code (flush ())] }
| _      { addc (Lexing.lexeme_char lexbuf 0); code lexbuf }

{
  let get lexbuf =
    init ();
    `Location (1, 1) :: text lexbuf
}
