open Aoc_2024.Utils

type calibrationEquation = {
  result:int;
  numbers:int list;
};;

let parseLine line =
  let split = line |> String.split_on_char ':' in
  let result = split |> List.hd |> int_of_string in
  let numbers = split |> List.tl |> List.hd |> String.split_on_char ' ' |> List.filter ((<>) "") |> List.map int_of_string in
  {result=result; numbers=numbers}
;;

let concat lhs rhs =
  int_of_string ((string_of_int lhs) ^ (string_of_int rhs))
;;

let rec isValid part2 cE =
  match cE.numbers with
  | [] -> cE.result == 0
  | x::[] -> cE.result == x
  | x::x'::xs -> 
    (isValid part2 {result=cE.result; numbers=(x*x')::xs}) || 
    (isValid part2 {result=cE.result; numbers=(x+x')::xs}) ||
    (part2 && (isValid part2 {result=cE.result; numbers=(concat x x')::xs}))
;;

let sol01 lines =
  lines
  |> List.map parseLine
  |> List.filter (isValid false)
  |> List.map (fun cE -> cE.result)
  |> List.fold_left (+) 0
;;

let sol02 lines =
  lines
  |> List.map parseLine
  |> List.filter (isValid true)
  |> List.map (fun cE -> cE.result)
  |> List.fold_left (+) 0
;;

let rec isValid cE =
  match cE.numbers with
  | [] -> cE.result == 0
  | x::[] -> cE.result == x
  | x::x'::xs -> (isValid {result=cE.result; numbers=(x*x')::xs}) || (isValid {result=cE.result; numbers=(x+x')::xs})
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/07/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;