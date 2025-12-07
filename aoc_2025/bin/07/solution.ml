open Aoc_2025.Utils

type tile = SPACE | SPLITTER | VISITED_SPLITTER of int

let tile_of_char = function
  | '.' -> SPACE
  | 'S' -> SPACE
  | '^' -> SPLITTER
  | _ -> VISITED_SPLITTER 0
;;

(* let char_of_tile = function
  | SPLITTER -> '^'
  | VISITED_SPACE -> '#'
  | VISITED_SPLITTER -> '1'
  | EMPTY_SPACE -> '.'
;; *)

let fire_beam start_x manifold =
  let height = manifold |> height in
  let rec imp x y =
    if y>=height then 1 else
      match manifold.(x).(y) with
      | SPACE -> imp x (y+1)
      | VISITED_SPLITTER n -> n
      | SPLITTER -> 
        let left = imp (x-1) y in
        let right = imp (x+1) y in
        let many_worlds = left + right in
        manifold.(x).(y) <- VISITED_SPLITTER many_worlds;
        many_worlds
  in
  imp start_x 1;
;;

let count_visited_splitters manifold =
  manifold
  |> Array.map (Array.fold_left (
    fun acc elem -> match elem with 
    | VISITED_SPLITTER _ -> acc+1
    | _ -> acc
  ) 0)
  |> Array.fold_left (+) 0
;;

let sol01 lines =
  let start_idx = String.index (List.nth lines 0) 'S' in
  let manifold = lines |> lines_to_matrix tile_of_char in
  let _ = fire_beam start_idx manifold in
  manifold |> count_visited_splitters
;;

let sol02 lines =
  let start_idx = String.index (List.nth lines 0) 'S' in
  let manifold = lines |> lines_to_matrix tile_of_char in
  fire_beam start_idx manifold
;;

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "bin/07/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
