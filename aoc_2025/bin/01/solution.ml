open Aoc_2025.Utils

let parse_instruction instruction = 
  let factor = match instruction.[0] with
    | 'L' -> -1
    | 'R' -> 1
    | _ -> raise (Failure "Wrong input")
    in
    factor * (int_of_string (String.sub instruction 1 (String.length instruction - 1)))
;;

let rec count_zeros_pt1 pos counter = function
  | [] -> counter
  | x::xs -> match (modulo (pos+x) 100) with 
    | 0 -> count_zeros_pt1 0 (counter+1) xs
    | n -> count_zeros_pt1  n (counter) xs
;; 

let sol01 lines =
  lines
  |> List.map parse_instruction
  |> (count_zeros_pt1 50 0)
;; 

let mirror n = modulo(-(abs n)) 100

let rec step pos distance = 
  if ((sign pos) == (sign distance)) then (
    let new_pos = pos + distance in
    (new_pos mod 100), ((abs new_pos) / 100)
  ) else (
    let new_mirrored_pos, counter = step (mirror pos) (abs distance) in
    (mirror new_mirrored_pos, counter)

  )
;;

let rec count_zeros_pt2 pos counter = function
  | [] -> counter
  | x::xs -> 
    let new_pos, increment = step pos x in
    count_zeros_pt2 new_pos (counter+increment) xs
  ;; 

let sol02 lines =
  lines
  |> List.map parse_instruction
  |> (count_zeros_pt2 50 0)
;; 

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/01/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
