open Aoc_2024.Utils

let width = 101;;
let height = 103;;

let parse_line line = 
  let split = line |> String.split_on_char ' ' |> List.map (String.split_on_char '=') in
  let p = List.nth (List.nth split 0) 1 |> coord_of_string in
  let v = List.nth (List.nth split 1) 1 |> coord_of_string in
  p,v
;;

let pos_after_n p v n =
  let x = (fst p + n * fst v) mod width in
  let y = (snd p + n * snd v) mod height in
  let x = if x < 0 then width + x else x in
  let y = if y < 0 then height + y else y in
  (x,y)
;;

let rec quadrant_numbers = function
  | [] -> (0,0,0,0)
  | (x,y)::rest -> let (q_0, q_1, q_2, q_3) = quadrant_numbers rest in
    if x < (width / 2) && y < (height / 2) then (q_0+1, q_1, q_2, q_3) else
    if x < (width / 2) && y > (height / 2) then (q_0, q_1+1, q_2, q_3) else
    if x > (width / 2) && y < (height / 2) then (q_0, q_1, q_2+1, q_3) else
    if x > (width / 2) && y > (height / 2) then (q_0, q_1, q_2, q_3+1) else
    (q_0, q_1, q_2, q_3)
;;

let sol01 lines = 
  lines
  |> List.map parse_line
  |> List.map (fun (p,v) -> pos_after_n p v 100)
  |> quadrant_numbers
  |> (fun (q_0, q_1, q_2, q_3) -> q_0 * q_1 * q_2 * q_3)
;;

let variance coord_list =
  let x_mean = (float_of_int width) /. 2.0 in
  let y_mean = (float_of_int height) /. 2.0 in
  let rec imp = function
    | [] -> (0.0,0.0)
    | (x,y)::rest -> 
      let x = float_of_int x in 
      let y = float_of_int y in 
      let (x,y) = ((x-.x_mean)*.(x-.x_mean)),((y-.y_mean)*.(y-.y_mean)) in
      let (x_acc,y_acc) = (imp rest) in (x+.x_acc,y+.y_acc)
  in
  let n = float_of_int (List.length coord_list) in
  let (x,y) = imp coord_list in (x/.n,y/.n)
;;

let sol02 lines = 
  let robots = lines |> List.map parse_line in
  let rec imp t =
    let (var_x, var_y) = robots |> List.map (fun (p,v) -> pos_after_n p v t) |> variance in
    if var_x < 350.0 && var_y < 650.0 then t else imp (t+1)
  in
  imp 0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/14/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;