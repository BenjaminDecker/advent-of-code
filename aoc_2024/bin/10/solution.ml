open Aoc_2024.Utils

let rec count_paths heightmap position =
  let current_height = heightmap#get_at position in
  if current_height == 9 then [position] else
  [N;E;S;W]
  |> List.to_seq
  |> (Seq.map (next_coords position))
  |> Seq.filter heightmap#is_valid
  |> Seq.filter (fun coord -> heightmap#get_at coord == (current_height + 1))
  |> Seq.map (count_paths heightmap)
  |> Seq.fold_left (@) []
;;

let sol01 lines = 
  let lines = 
    lines 
    |> List.map String.to_seq 
    |> List.map (Seq.map int_of_char) 
    |> List.map (Seq.map (fun c -> c - (int_of_char '0')))
    |> List.map List.of_seq
  in
  let heightmap = new flat_array lines in
  heightmap#find_all_coords ((=) 0) |> Seq.map (count_paths heightmap) |> Seq.map (List.sort_uniq smallerEqual) |> Seq.map List.length |> Seq.fold_left (+) 0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/10/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  (* sol02 lines |> print_int |> print_newline; *)
;;