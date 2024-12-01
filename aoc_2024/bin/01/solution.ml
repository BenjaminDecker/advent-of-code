
let nth_int n line =
  int_of_string (List.nth ( List.filter (fun x -> x <> "") (String.split_on_char ' ' line)) n)
;;

let line_to_int_list line =
  String.split_on_char ' ' line |> List.filter (fun x -> x <> "") |> List.map int_of_string
;;

let lines_to_int_list_list lines = 
  List.map line_to_int_list lines
;;

let list_of_nth_ints n lines = 
  List.map (nth_int n) lines
;;

let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows -> List.map List.hd rows :: transpose (List.map List.tl rows)
;;

let get_difference int_list =
  let left = List.nth int_list 0 in
  let right = List.nth int_list 1 in
  abs (left-right)
;;

let sol01 lines =
  lines_to_int_list_list lines
  |> transpose
  |> List.map (List.sort compare)
  |> transpose
  |> List.map get_difference
  |> List.fold_left (+) 0
;;

let rec count_occurences list elem = 
  match list with
  | [] -> 0
  | x::xs -> if x = elem then
    1 + count_occurences xs elem
  else
    count_occurences xs elem
;;

let sol02 lines =
  let left = list_of_nth_ints 0 lines in
  let right = list_of_nth_ints 1 lines in
  List.map (fun elem -> elem * count_occurences right elem) left
  |> List.fold_left (+) 0

let () = 
let lines = Aoc_2024.Utils.read_lines "bin/01/input.txt" in
print_endline "Solution 1:";
lines
  |> sol01
  |> string_of_int
  |> print_endline;

print_endline "Solution 2:";
lines
  |> sol02
  |> string_of_int
  |> print_endline
;;
