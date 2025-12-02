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


let sign x = if x<0 then -1 else 1;;

let rec num_digits = function
  | 0 -> 0
  | n -> 1 + num_digits (n/10)
;;

let divmod n d = n/d, n mod d;;

let rec pow b = function
| 0 -> 1
| e -> b * pow b (e-1)
;;

let print_list f = function
  | [] -> print_endline "[]";
  | l ->
    let rec imp = function
      | [] -> ()
      | x::xs -> print_string ", "; print_string x; imp xs
    in
    let l = l |> List.map f in
    print_string "[";
    print_string (List.hd l);
    imp (List.tl l);
    print_endline "]"
;;

let range a b =
  let rec imp l i = if i < a then l else imp (i::l) (i-1) in
imp [] (b-1)
;;
