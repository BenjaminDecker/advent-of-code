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
  let big_1 = Big_int_Z.big_int_of_int 1 in
  if depth == 0 then [big_1] else
  try sub_list (depth + 1) (Hashtbl.find h stone)
  with _ ->
  let result = big_1::(
    if stone == 0 then count_stones (depth-1) 1 else
      match split_int stone with
      | None -> count_stones (depth-1) (stone * 2024)
      | Some (left, right) -> List.fold_right2 (fun elem1 elem2 acc -> (Big_int_Z.add_big_int elem1 elem2)::acc) (count_stones (depth-1) left) (count_stones (depth-1) right) []
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
  stones |> List.map (count_stones 25) |> List.map get_last |> List.fold_left Big_int_Z.add_big_int Big_int_Z.zero_big_int
;;

let sol02 lines =
  let stones = lines |> List.hd |> parse_line in
  stones |> List.map (count_stones 75) |> List.map get_last |> List.fold_left Big_int_Z.add_big_int Big_int_Z.zero_big_int
;;

let sol03 lines =
  let stones = lines |> List.hd |> parse_line in
  stones |> List.map (count_stones 1000) |> List.map get_last |> List.fold_left Big_int_Z.add_big_int Big_int_Z.zero_big_int
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/11/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> Big_int_Z.string_of_big_int |> print_endline;
  print_endline "Solution 2:";
  sol02 lines |> Big_int_Z.string_of_big_int |> print_endline;
  print_endline "Solution 3:";
  sol03 lines |> Big_int_Z.string_of_big_int |> print_endline;
;;