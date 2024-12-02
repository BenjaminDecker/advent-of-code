let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let line_to_int_list line =
  String.split_on_char ' ' line |> List.filter (fun x -> x <> "") |> List.map int_of_string
;;

let lines_to_int_list_list lines = 
  List.map line_to_int_list lines
;;