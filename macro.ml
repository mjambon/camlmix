let read_chan f ic =
  try
    while true do
      f (input_char ic)
    done
  with End_of_file -> ()

let include_file file =
  let ic = open_in_bin file in
  read_chan print_char ic;
  close_in ic

let html_verbatim file =
  let ic = open_in_bin file in
  print_string "<pre>\n";
  read_chan 
    (function
	   | '<' -> print_string "&lt;"
       | '>' -> print_string "&gt;"
       | '&' -> print_string "&amp;"
       | c   -> print_char c)
    ic;
  print_string "</pre>\n";
  close_in ic

let camlmix file = Sys.command ("./camlmix " ^ file)

let author = "Martin Jambon"
