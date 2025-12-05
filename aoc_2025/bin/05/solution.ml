open Aoc_2025.Utils

let parse_range line =
  line
  |> String.split_on_char '-'
  |> List.map int_of_string
  |> (fun r -> (List.hd r), (List.hd (List.tl r)))
;;

let is_fresh ranges id =
  ranges
  |> List.find_opt (fun r -> (id >= (fst r)) && (id <= (snd r)))
  |> Option.is_some
;;

let parse_input lines =
  let separator_idx = lines |> List.find_index (fun l -> String.length l == 0) |> Option.get in
  let ranges = lines |> List.take separator_idx |> List.map parse_range in
  let ids = lines |> List.drop (separator_idx+1) |> List.map int_of_string in
  ranges, ids
;;

let sol01 lines =
  let ranges, ids = lines |> parse_input in
  ids
  |> List.filter (is_fresh ranges)
  |> List.length
;;

let rec merge_ranges acc ranges =
  match (acc, ranges) with
  | [],[] -> []
  | [], (x::xs) -> merge_ranges [x] xs
  | (x::xs), [] -> (x::xs)
  | (acc_hd::acc_rest), (ranges_hd::ranges_rest) -> (
    if (fst ranges_hd) <= (snd acc_hd) then (
      let new_acc_hd = (fst acc_hd), (max (snd acc_hd) (snd ranges_hd)) in
      merge_ranges (new_acc_hd::acc_rest) ranges_rest
    ) else (
      merge_ranges (ranges_hd::acc) ranges_rest
    )
  )
;;

let sol02 lines =
  let ranges, _ = lines |> parse_input in
  ranges
  |> List.sort (fun lhs rhs -> compare (fst lhs) (fst rhs))
  |> merge_ranges []
  |> List.map (fun r -> (snd r) - (fst r) + 1)
  |> List.fold_left (+) 0
;;

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "bin/05/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
