open Aoc_2024.Utils

let parseRule line =
  line
  |> String.split_on_char '|'
  |> List.map int_of_string
  |> (fun split -> List.hd split, List.hd (List.tl split))
;;

let addRule rules rule =
  rules.(fst rule) <- (snd rule)::(rules.(fst rule));
  rules
;;

let getRules ruleLines = 
  ruleLines
  |> List.map parseRule
  |> List.fold_left addRule (Array.make 100 [])
;;

let parseUpdate line = 
  String.split_on_char ',' line
  |> List.map int_of_string
;;

let isCorrect rules update =
  fst (List.fold_left (
    fun acc elem -> match acc with
    | (false, _) -> (false, [])
    | (true, forbidden) -> (not (List.mem elem forbidden)), (rules.(elem) @ forbidden)
  ) (true, []) (List.rev update))

let smallerEqual rules lhs rhs = 
  Bool.to_int (List.mem rhs rules.(lhs))
;;

let createSum updates = 
  updates
  |> List.map (fun update -> List.nth update ((List.length update)/2))
  |> List.fold_left (+) 0

let sol lines = 
  let ruleLines = lines |> List.filter (String.exists ((=) '|')) in
  let updateLines = lines |> List.filter (String.exists ((=) ',')) |> List.map parseUpdate in
  let rules = getRules ruleLines in

  let sol01 = updateLines
  |> List.filter (isCorrect rules)
  |> createSum in

  let sol02 = updateLines
  |> List.filter (fun update -> not (isCorrect rules update))
  |> List.map (List.sort (smallerEqual rules))
  |> createSum in

  sol01, sol02
;;

let () = 
  let lines = read_lines "bin/05/input.txt" in
  let (sol01, sol02) = lines |> sol in
  print_endline "Solution 1:";
  sol01 |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 |> print_int |> print_newline;
;;