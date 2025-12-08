open Aoc_2025.Utils

let parse_coordinates line =
  let coords = line |> String.split_on_char ',' |> List.map float_of_string in
  (List.nth coords 0), (List.nth coords 1), (List.nth coords 2)
;;

let distance (x1, y1, z1) (x2, y2, z2) = 
  Float.sqrt ((x1-.x2)*.(x1-.x2) +. (y1-.y2)*.(y1-.y2) +. (z1-.z2)*.(z1-.z2))
;;

let create_sorted_connection_list coordinates =
  let rec imp = function
    | [] -> []
    | (i, x)::xs -> (
      let acc = imp xs in
      xs
      |> List.fold_left (fun acc (i', elem) -> ((i, i'), (distance x elem))::acc) acc
  ) in
  imp (coordinates |> List.mapi (fun i elem -> (i, elem)))
  |> List.sort (fun (_, distance1) (_, distance2) -> compare distance1 distance2)
  |> List.map fst
;;

module IntSet = Set.Make(Int);;

type status = DISCONNECTED of IntSet.t list | CONNECTED of int * int

let create_new_connection current_connections (idx1, idx2) =
  let included_in, not_included_in =
    current_connections
    |> List.partition (fun group -> IntSet.mem idx1 group || IntSet.mem idx2 group)
  in
  let fused_group =
    included_in
    |> List.fold_left IntSet.union IntSet.empty
    |> IntSet.add idx1
    |> IntSet.add idx2
  in
  if (fused_group |> IntSet.cardinal) = 1000 then CONNECTED (idx1, idx2) else
  DISCONNECTED (fused_group::not_included_in)
;;

let sol01 lines =
  lines
  |> List.map parse_coordinates
  |> create_sorted_connection_list
  |> List.take 1000
  |> List.fold_left (
    fun acc elem -> match create_new_connection acc elem with
    | CONNECTED _ -> failwith "Uff"
    | DISCONNECTED new_acc -> new_acc
  ) []
  |> List.map IntSet.cardinal
  |> List.sort compare
  |> List.rev
  |> List.take 3
  |> List.fold_left ( * ) 1
;;

let sol02 lines =
  let coordinates = lines |> List.map parse_coordinates in
  let rec imp current_connections = function
  | [] -> failwith "Uff"
  | x::xs -> match create_new_connection current_connections x with
    | DISCONNECTED current_connections -> imp current_connections xs
    | CONNECTED (idx1, idx2) -> (idx1, idx2)
  in
  let (idx1, idx2) = imp [] (coordinates |> create_sorted_connection_list) in
  let (x1, _, _) = (List.nth coordinates idx1) in
  let (x2, _, _) = (List.nth coordinates idx2) in
(int_of_float x1) * (int_of_float x2)
;;

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "bin/08/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
