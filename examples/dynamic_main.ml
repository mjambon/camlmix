let _ =
  match Sys.argv with
      [| _; param |] -> Dynamic.render param
    | _ -> prerr_string "Usage: dynamic PARAM\n"
