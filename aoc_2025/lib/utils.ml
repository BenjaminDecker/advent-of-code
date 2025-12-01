let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;



let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y
;;


let sign x = if x<0 then -1 else 1