open Aoc_2024.Utils

module Coord = struct 
  type t = int * int
  let compare = compare
end

module CoordSet = Set.Make(Coord)

let get_region farm coord =
  let plant_type = farm#get_at coord in
  let rec imp w_set f_set =
    match CoordSet.choose_opt w_set with
    | None -> f_set
    | Some coord -> (
      let w_set = CoordSet.remove coord w_set in
      let neighbors = farm#get_neighbor_coords [N;E;S;W] coord |> List.filter (fun coord -> farm#get_at coord = plant_type) |> CoordSet.of_list in
      let neighbors = CoordSet.diff neighbors f_set in
      let w_set = CoordSet.union w_set neighbors in
      let f_set = CoordSet.add coord f_set in
      imp w_set f_set
    )
  in
  imp (CoordSet.singleton coord) CoordSet.empty
;;

let get_regions lines =
  let lines = lines |> List.map String.to_seq |> List.map List.of_seq in
  let farm = new flat_array lines in
  let all_coords = farm#find_all_coords (fun _ -> true) |> CoordSet.of_seq in
  let rec imp w_set acc =
    match CoordSet.choose_opt w_set with
    | None -> acc
    | Some coord -> (
      let region = get_region farm coord in
      let w_set = CoordSet.diff w_set region in
      imp w_set (region::acc)
    )
  in
  imp all_coords []
;;

let does_count region coord = function
  | N -> (CoordSet.mem (next_coords coord NE) region) || (not (CoordSet.mem (next_coords coord E) region))
  | E -> (CoordSet.mem (next_coords coord SE) region) || (not (CoordSet.mem (next_coords coord S) region))
  | S -> (CoordSet.mem (next_coords coord SW) region) || (not (CoordSet.mem (next_coords coord W) region))
  | W -> (CoordSet.mem (next_coords coord NW) region) || (not (CoordSet.mem (next_coords coord N) region))
  | _ -> raise (Failure "Diagonal directions are not allowed")
;;

let get_perimeter pt1 region =
  CoordSet.fold (fun coord acc -> 
    [N;E;S;W]
    |> List.filter (fun dir -> (not (CoordSet.mem (next_coords coord dir) region)))
    |> List.filter (fun dir -> pt1 || (does_count region coord dir))
    |> List.length
    |> (+) acc
  ) region 0
;;

let sol01 lines = 
  lines |> get_regions |> List.map (fun region -> (get_perimeter true region) * (CoordSet.cardinal region)) |> List.fold_left (+) 0
;;

let sol02 lines = 
  lines |> get_regions |> List.map (fun region -> (get_perimeter false region) * (CoordSet.cardinal region)) |> List.fold_left (+) 0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/12/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;