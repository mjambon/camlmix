let _ =
  match Sys.argv with
      [| _; first; last; age_s |] -> 
	let buf = Buffer.create 1000 in
	let print = Buffer.add_string buf in
	Dynamic2.render ~print (first, last, int_of_string age_s);
	let contents = Buffer.contents buf in
	print_string (String.uppercase contents)

    | _ -> prerr_string "Usage: dynamic2 firstname lastname age\n"; exit 1
