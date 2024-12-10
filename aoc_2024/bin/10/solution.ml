open Aoc_2024.Utils

let rec count_paths heightmap position =
  let current_height = heightmap#get_at position in
  if current_height == 9 then ([position], 1) else
  [N;E;S;W]
  |> List.to_seq
  |> (Seq.map (next_coords position))
  |> Seq.filter heightmap#is_valid
  |> Seq.filter (fun coord -> heightmap#get_at coord == (current_height + 1))
  |> Seq.map (count_paths heightmap)
  |> Seq.fold_left (fun (foundCoords, numPaths) (foundCoords', numPaths') -> (foundCoords' @ foundCoords, numPaths' + numPaths)) ([], 0)
;;

let sol lines = 
  let lines = 
    lines 
    |> List.map String.to_seq 
    |> List.map (Seq.map int_of_char) 
    |> List.map (Seq.map (fun c -> c - (int_of_char '0')))
    |> List.map List.of_seq
  in
  let heightmap = new flat_array lines in
  heightmap#find_all_coords ((=) 0)
  |> Seq.map (count_paths heightmap)
  |> Seq.map (fun (foundCoords, numPaths) -> (foundCoords |> (List.sort_uniq smallerEqual) |> List.length), numPaths)
  |> Seq.fold_left (fun (uniqueCoords, numPaths) (uniqueCoords', numPaths') -> (uniqueCoords + uniqueCoords', numPaths + numPaths')) (0,0)
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/10/input.txt" in
  let sol = sol lines in
  print_endline "Solution 1:";
  (fst sol) |> print_int |> print_newline;
  print_endline "Solution 2:";
  (snd sol) |> print_int |> print_newline;
;;