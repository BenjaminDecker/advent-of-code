open Aoc_2025.Utils

let parse_point line =
  let line = line |> String.split_on_char ',' |> List.map int_of_string in
  (line |> List.hd), (line |> List.tl |> List.hd)
;;

let rectangle_size (x1, y1) (x2, y2) = ((abs (x1 - x2)) + 1) * ((abs (y1 - y2)) + 1);;

let largest_area_rectangle points =
  points
  |> Seq.unfold (
    function
    | [] -> None
    | p1::xs ->
      let s =xs
      |> List.to_seq
      |> Seq.map (fun p2 -> rectangle_size p1 p2) in
      Some (s, xs)
  )
  |> Seq.concat
  |> Seq.fold_left max 0
;;

let sol01 lines =
  lines
  |> List.map parse_point
  |> largest_area_rectangle
;;

let sol02 lines =
  lines
  |> List.length
;;

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "bin/09/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
