open Printf
open Arg

let sources     = ref []
let parse_only  = ref false
let ml_name     = ref None
let fun_option  = ref false
let clean       = ref false
let ocaml       = ref "ocaml"
let insertions  = ref []
let stdout_file = ref None
let remix       = ref false

let usage_msg = "\
General rules:
Text between ## and ## is considered as a block of code.
A block of code is normally some OCaml code. It can be any
kind of toplevel expressions such as definitions or application
of printing functions.

The document blocks are printed on stdout.
To avoid interpretation of repeated # symbols as OCaml quotations
they must be prefixed by an additional # symbol. E.g.,
### is replaced by ## both in code and in document blocks.

White space introduced by OCaml quotations can be controlled
by ##. and .## quotation symbols:
    ##. causes any white space to the left of the quotation symbol
        to be omitted from the output. Similarly
    .## causes any white space followed by one newline
        to be omitted from the output.

Directives:
Blocks of code that start with the @ character (possibly preceded 
by spaces) are directives for Camlmix.
The following directives are available:
  include \"somefile.mlx\"; (* inserts a camlmix file *)
  skip;                   (* ignores the next block of text *)

e.g.: Hello ## @include \"world.mlx\"; skip .## ## let x = 123 ##!


Advanced usage:
The OCaml program that is generated from the Camlmix files first defines
a module called Camlmix. This module contains several variables that are 
updated automatically and may be used explicitely by the user.

module Camlmix :
  sig
    val source : string ref
    val line   : int ref
    val char   : int ref        
        (* source, line and char refer to the location of the first 
           character of the current block in its source file
           (source file, line number starting from 1, position in the line
           starting from 1). They are updated automatically at the beginning
           of each (code or text) block. *)

    val printer : (string -> unit) ref 
        (* the function that prints text blocks *)

    val print_with : (string -> unit) -> unit
        (* print_with f prints the next block of text using f instead
           of the current printer. 
           Its behavior is undefined if it is called several 
           times in the same OCaml block. *)

    val print_if : bool -> unit
        (* print_if cond prints the next block of text only 
           if cond is true. It uses print_with. *)
  end


Usage: camlmix [options] file1 file2 ... fileN

A temporary file \"fileN.ml\" is created and then executed
with ocaml.

Command line options:"

let options = [
  "-c",
  Set parse_only,
  " only generate the .ml file";
  
  "-co",
  String (fun s -> ml_name := Some s),
  "<filename>  specify a name for the .ml file";
  
  "-fun",
  Set fun_option,
  " make it the body of a function named \"render\", \
    using a single argument named \"param\"";
  
  "-e",
  Set_string ocaml,
  "<ocaml>  specify ocaml executable";

  "-o",
  String (fun s -> stdout_file := Some s),
  "<file>  specify an output file";

  "-clean",
  Set clean,
  " remove the temporary .ml file after execution";
  
  "-insert",
  String (fun s -> insertions := s :: !insertions),
  "<ocaml code>  insert this code first";
  
  "-remix",
  Set remix,
  " try a conversion to the camlremix syntax";

  "-version",
  Unit (fun () -> print_endline Version.version; exit 0),
  "  prints the version of Camlmix and exits";
]

let add_source s = sources := s :: !sources

let camlmix_hooks oc =
  fprintf oc "\
module Camlmix =
struct
  let source = ref \"\"
  let line = ref 1
  let char = ref 1
  let printer = ref print
  let print_with f =
    let saved_printer = !printer in
    printer := (fun s -> f s; printer := saved_printer)
  let print_if test =
    if not test then print_with ignore
end
;;
";
  List.iter
    (fun s -> fprintf oc "%s\nlet () = ();;\n" s) 
    (List.rev !insertions)


let rec process_file oc grand_parents ((in_file, digest) as file_info) =
  let parents = file_info :: grand_parents in
  let (update_location, get_location, print_location) =
    let source = ref "" 
    and line = ref 1
    and char = ref 1 in
    let update s l c = 
      source := s;
      line := l;
      char := c
    and get () = (!source, !line, !char)
    and print ?(dline = 0) ?(dchar = 0) oc =
      fprintf oc 
	"\n# %i %S;;\n%s" 
	(!line + dline) !source 
	(String.make (!char + dchar - 1) ' ') in
    (update, get, print) in
      
  let ic = open_in_bin in_file in
  let lexbuf = Lexing.from_channel ic in
  let list = Lexer.get lexbuf in
  close_in ic;

  update_location in_file 1 1;
  print_location oc;
  let rec iter skip_text = function
      [] -> ()
    | `Text s :: rest -> 
	if not skip_text then
	  (fprintf oc "\nlet () = !Camlmix.printer ";
	   print_location ~dchar:1 oc;
	   fprintf oc "%S;;\n" s);
	iter false rest

    | `Code s :: rest ->
	let loc = get_location () in
	(match Directive.find_directives parents loc s with
	     Some l ->
	       let skip_next_text = 
		 expand_directives oc parents l in
	       iter skip_next_text rest
	   | None ->
	       (match Directive.find_expr loc s with
		    Some (e, dchar) -> 
		      fprintf oc "\nlet () = !Camlmix.printer ";
		      print_location ~dchar:(dchar - 2) oc;
		      fprintf oc "( %s : string);;\n" e
		  | None ->
		      print_location oc;
		      fprintf oc "%s" s);
	       iter false rest)
	       
    | `Location (line, char) :: rest -> 
	update_location in_file line char;
	fprintf oc "
let () =
  Camlmix.line := %i;
  Camlmix.char := %i;
  Camlmix.source := %S
;;
"
	     line char in_file;
	iter skip_text rest in 

    iter false list


and expand_directives oc parents l =
  let skip = ref false in
  List.iter
    (function
	 `Include (file, digest) -> 
	   process_file oc parents (file, digest)
       | `Skip -> skip := true)
    l;
  !skip


let render_body wrap body oc =
  if wrap then
    fprintf oc "\
let render ?(print = fun s -> print_string s; flush stdout) param =
let module M = struct
%t
end in
()
"
      body
  else 
    fprintf oc "\
let print s = print_string s; flush stdout
%t
"
      body


let protect_remix oc s =
  String.iter
    (function
	 '#' -> output_string oc "##"
       | '~' -> output_string oc "~~"
       | c -> output_char oc c)
    s

let remix_file in_file =
  let out_file = in_file ^ ".remix" in
  let oc = open_out out_file in
  let ic = open_in_bin in_file in
  let lexbuf = Lexing.from_channel ic in
  let list = Lexer.get lexbuf in
  close_in ic;

  List.iter 
    (function
	 `Text s -> protect_remix oc s
       | `Code s -> fprintf oc "#(%a)#" protect_remix s
       | `Location (line, char) -> ())
    list;

  close_out oc;
  printf "Wrote %s\n" out_file



let exec_ocaml out_file ml_file =
  let buf = Buffer.create 1000 in
  let ic = Unix.open_process_in (sprintf "%s %s" !ocaml ml_file) in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file ->
     let finish () = if !clean then Sys.remove ml_file in
     match Unix.close_process_in ic with
	 Unix.WEXITED 0 -> finish ()
       | Unix.WEXITED n -> finish (); exit n
       | _ -> finish (); exit 2);

  match out_file with
      None -> Buffer.output_buffer stdout buf
    | Some file -> 
	let oc = open_out file in
	Buffer.output_buffer oc buf;
	close_out oc

let _ =
  parse options add_source usage_msg;
  let rev_in_files = !sources in
  match rev_in_files, !stdout_file with
      [], None -> ()
    | [], Some file -> close_out (open_out file)
    | l, opt ->
	if !remix then
	  List.iter remix_file l
	else
	  let ml_file = 
	    match !ml_name with
		Some s -> s
	      | None -> List.hd l ^ ".ml" in
	  let oc = open_out ml_file in
	  let body oc =
	    camlmix_hooks oc;
	    List.iter (process_file oc []) 
	      (List.rev_map 
		 (fun file -> (file, lazy (Digest.file file))) rev_in_files) in
	  render_body !fun_option body oc;
	  close_out oc;
	  if !parse_only then
	    exit 0
	  else
	    exec_ocaml opt ml_file
