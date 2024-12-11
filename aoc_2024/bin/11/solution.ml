open Aoc_2024.Utils

let parse_line line =
  line |> String.split_on_char ' ' |> List.map int_of_string
;;

let split_int n =
  let num_digits = numDigits n in
  if num_digits mod 2 != 0 then None else
  let s = pow 10 (num_digits/2) in
  Some (n / s,n mod s)
;;

let h = Hashtbl.create 0;;

let rec sub_list depth l = 
  if depth == 0 then [] else match l with
  | [] -> raise (Failure "List too short")
  | x::xs -> x::(sub_list (depth-1) xs)
;;  

let rec count_stones depth stone =
  if depth == 0 then [1] else
  try sub_list (depth + 1) (Hashtbl.find h stone)
  with _ ->
  let result = 1::(
    if stone == 0 then count_stones (depth-1) 1 else
      match split_int stone with
      | None -> count_stones (depth-1) (stone * 2024)
      | Some (left, right) -> List.fold_right2 (fun elem1 elem2 acc -> (elem1 + elem2)::acc) (count_stones (depth-1) left) (count_stones (depth-1) right) []
  ) in
  Hashtbl.add h stone result;
  result
;;

let rec get_last = function
  | [] -> raise (Failure "Empty list")
  | x::[] -> x
  | _::xs -> get_last xs
;;

let sol01 lines =
  let stones = lines |> List.hd |> parse_line in
  stones |> List.map (count_stones 25) |> List.map get_last |> List.fold_left (+) 0
;;

let sol02 lines =
  let stones = lines |> List.hd |> parse_line in
  stones |> List.map (count_stones 75) |> List.map get_last |> List.fold_left (+) 0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/11/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;