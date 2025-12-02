open Aoc_2025.Utils

let next_repeated p = p |> List.map (fun x -> x+1);;

let int_of_repeated p = 
  p
  |> List.rev
  |> List.fold_left (fun acc elem -> acc + elem * (pow 10 (num_digits acc))) 0
;;

let rec next_repeated_of_int num_repetitions i = 
  let digits = num_digits i in
  match divmod digits num_repetitions with
  | (pal_width, 0) -> 
    let most_significant_part = i / (pow 10 ((num_repetitions-1) * pal_width)) in
    let smaller_pal = List.init num_repetitions (fun _ -> most_significant_part) in
    let larger_pal = List.init num_repetitions (fun _ -> most_significant_part+1) in
    if (int_of_repeated smaller_pal)<i then larger_pal else smaller_pal
  | _ -> next_repeated_of_int num_repetitions (pow 10 (num_digits i))
;;

let count_invalids num_repetitions from until =
  let from_p = next_repeated_of_int num_repetitions from in
  let until_p = next_repeated_of_int num_repetitions until in
  let rec imp from until invalids =
    if (List.equal (==) from until) then invalids else (
      imp (next_repeated from) until ((int_of_repeated from)::invalids) 
    )
  in
  let invalids = imp from_p until_p [] in
  let invalids = if (int_of_repeated until_p) == until then until::invalids else invalids in
  invalids
;;

let parse_intervals line = 
  line
  |> String.split_on_char ','
  |> List.map (String.split_on_char '-')
  |> List.map (List.map int_of_string)
;;

let sol01 lines = 
  lines
  |> List.hd
  |> parse_intervals
  |> List.map (fun l -> count_invalids 2 (List.hd l) (List.hd (List.tl l)))
  |> List.map (List.fold_left (+) 0)
  |> List.fold_left (+) 0
;;

let sol02 lines = 
  lines
  |> List.hd
  |> parse_intervals
  |> List.map ( fun l -> 
      let from = List.hd l in
      let until = List.hd (List.tl l) in
      range 2 ((num_digits until)+1)
      |> List.map (fun i -> count_invalids i from until)

    )
  |> List.fold_left List.append []
  |> List.fold_left List.append []
  |> List.sort_uniq compare
  |> List.fold_left (+) 0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/02/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
