let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y
;;


let sign x = if x<0 then -1 else 1;;

let rec num_digits = function
  | 0 -> 0
  | n -> 1 + num_digits (n/10)
;;

let divmod n d = n/d, n mod d;;

let rec pow b = function
| 0 -> 1
| e -> b * pow b (e-1)
;;

let print_list f = function
  | [] -> print_endline "[]";
  | l ->
    let rec imp = function
      | [] -> ()
      | x::xs -> print_string ", "; print_string x; imp xs
    in
    let l = l |> List.map f in
    print_string "[";
    print_string (List.hd l);
    imp (List.tl l);
    print_endline "]"
;;

let range_seq a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;

let range a b = range_seq a b |> List.of_seq;;

let rec int_of_list acc = function
  | [] -> acc
  | x::xs -> (int_of_list (acc * 10 + x) xs)
;;

let lines_to_matrix f lines =
  Array.init_matrix
  (String.length (List.hd lines))
  (List.length lines)
  (fun x y -> f (String.get (List.nth lines y) x))
;;

let width m = Array.length m;;
let height m = Array.length (m.(0));;

let print_matrix f m =
  let width = width m in
  let height = height m in
  (range_seq 0 height) |> Seq.iter (fun y -> (range_seq 0 width) |> Seq.iter (fun x -> 
    m.(x).(y) |> f |> print_char;
  ); print_newline ();)
;;

type direction = N | E | S | W | NE | SE | SW | NW

let neighbors = [N; E; S; W; NE; SE; SW; NW]

let move_in_direction (x, y) = function
  | N -> (x, y - 1)
  | E -> (x + 1, y)
  | S -> (x, y + 1)
  | W -> (x - 1, y)
  | NE -> (x + 1, y - 1)
  | SE -> (x + 1, y + 1)
  | SW -> (x - 1, y + 1)
  | NW -> (x - 1, y - 1)
;;

let get_opt m (x, y) =
  let width = width m in
  let height = height m in
  if x<0 || x>=width || y<0 || y>=height then None else Some(m.(x).(y))
;;

let find_all_neighbor_coords f m coord =
  neighbors
  |> List.map (move_in_direction coord)
  |> List.filter (fun coord -> Option.is_some (get_opt m coord))
  |> List.filter (fun coord -> f (m.(fst coord).(snd coord)))
;;

let find_all_neighbors f m coord =
  neighbors
  |> List.map (move_in_direction coord)
  |> List.filter_map (get_opt m)
  |> List.filter f
;;

let coord_seq width height =
  Seq.unfold (fun (x,y) -> if y==height then None else (
    if x<(width-1) then Some((x,y), (x+1,y)) else Some((x,y), (0,y+1))
  )) (0,0)
;;

let string_of_coord (x,y) =
  "(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ")"
;;

let count_all f m =
  m
  |> Array.map (fun line ->
    line
    |> Array.map (fun elem -> if (f elem) then 1 else 0)
    |> Array.fold_left (+) 0
  )
  |> Array.fold_left (+) 0
;;