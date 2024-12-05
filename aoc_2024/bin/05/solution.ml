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
    | (true, forbidden) -> (not (List.exists ((=) elem) forbidden)), (rules.(elem) @ forbidden)
  ) (true, []) (List.rev update))

let sol01 lines = 
  let ruleLines = lines |> List.filter (String.exists ((=) '|')) in
  let updateLines = lines |> List.filter (String.exists ((=) ',')) in
  let rules = getRules ruleLines in
  updateLines
  |> List.map parseUpdate
  |> List.filter (isCorrect rules)
  |> List.map (fun l -> List.nth l ((List.length l)/2))
  |> List.fold_left (+) 0
;;

let sol02 _ = 
  0
;;


let () = 
  let lines = read_lines "bin/05/input.txt" in
  print_endline "Solution 1:";
  lines |> sol01 |> print_int |> print_newline;
  print_endline "Solution 2:";
  lines |> sol02 |> print_int |> print_newline;
;;
