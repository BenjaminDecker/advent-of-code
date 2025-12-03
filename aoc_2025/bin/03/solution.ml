open Aoc_2025.Utils

let parse_line line = 
  line
  |> String.to_seq
  |> Seq.map int_of_char
  |> Seq.map (fun i -> i-48)
  |> List.of_seq
;;

let joltage num_batteries batteries =
  let remaining = batteries |> List.drop ((batteries |> List.length)-num_batteries) in
  let batteries = batteries |> List.take ((batteries |> List.length)-num_batteries) in
  let rec imp batteries = function
  | [] -> []
  | x::xs ->
    let batteries = batteries @ [x] in
    let max_value = batteries |> List.fold_left (max) 0 in
    let index = batteries |> List.find_index (fun elem -> elem==max_value) |> Option.get in
    max_value::(imp (batteries |> List.drop (index+1)) xs)
  in
  imp batteries remaining
  ;;
;;

let total_joltage num_batteries lines =
  lines
  |> List.map parse_line
  |> List.map (joltage num_batteries)
  |> List.map (int_of_list 0)
  |> List.fold_left (+) 0
;;

let sol01 lines = lines |> total_joltage 2;;

let sol02 lines = lines |> total_joltage 12;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/03/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
