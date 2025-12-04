open Aoc_2025.Utils

type box = BOX | NO_BOX

let box_of_char = function
  | '@' -> BOX
  | _ -> NO_BOX
;;

(* let char_of_box = function
  | BOX -> '@'
  | NO_BOX -> '.'
;; *)

let remove_boxes m =
  Array.init_matrix
  (width m)
  (height m)
  (fun x y ->
    (x,y)
    |> (find_all_neighbors (fun b -> b==BOX) m)
    |> List.length
    |> (fun len -> if len < 4 then NO_BOX else m.(x).(y))
  )
;;

let sol01 lines =
  let m = lines |> (lines_to_matrix box_of_char) in
  let m_removed = m |> remove_boxes in
  (count_all (fun b -> b==BOX) m ) - (count_all (fun b -> b==BOX) m_removed)
;;

let sol02 lines = 
  lines
  |> (lines_to_matrix box_of_char)
  |> Seq.unfold (fun old_m -> 
    let new_m = old_m |> remove_boxes in
    let removed_boxes = (old_m |> count_all (fun b -> b==BOX)) - (new_m |> count_all (fun b -> b==BOX)) in
    if removed_boxes==0 then None else Some(removed_boxes, new_m)
  )
  |> Seq.fold_left (+) 0
;;


let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/04/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
