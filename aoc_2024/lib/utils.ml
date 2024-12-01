let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;