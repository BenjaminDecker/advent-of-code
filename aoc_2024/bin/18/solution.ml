open Aoc_2024.Utils

let width = 71;;
let height = width;;

type tile = Empty | Corrupted;;

let rec dijkstra grid visited = function
  | [] -> failwith "No path found"
  | ((70,70), d)::_ -> d
  | (coord, d)::rest -> 
    let neighbors =
      grid#get_neighbor_coords [N;E;S;W] coord
      |> List.filter (fun neighbor -> not (CoordSet.mem neighbor visited))
      |> List.filter (fun neighbor -> (grid#get_at neighbor) = Empty )
      |> List.map (fun neighbor -> (neighbor, d+1)) in
    let unvisited = 
      (neighbors @ rest)
      |> List.sort (fun lhs rhs -> compare (snd lhs) (snd rhs))
      |> List.sort_uniq (fun lhs rhs -> compare (fst lhs) (fst rhs))
      |> List.sort (fun lhs rhs -> compare (snd lhs) (snd rhs)) in
    dijkstra grid (CoordSet.add coord visited) unvisited
;;

let sol01 lines =
  let grid = List.init height (fun _ -> List.init width (fun _ -> Empty)) |> new flat_array in
  lines 
  |> List.map coord_of_string
  |> List.iter (fun coord -> grid#put_at coord Corrupted);
  dijkstra grid CoordSet.empty [((0,0),0)]
;;

let binary_search f arr =
  let rec imp f arr left right =
    if left = right then arr.(left) else
    let middle = (left+right)/2 in
    if f arr.(middle) then imp f arr left middle else imp f arr (middle+1) right
  in
  imp f arr 0 (Array.length arr)
;;

let sol02 lines = 
  lines
  |> List.length
  |> (range 0)
  |> Array.of_list
  |> binary_search (fun i -> try let _ = sol01 (sublist (i+1) lines) in false with _ -> true)
  |> List.nth lines
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/18/input.txt" in
  print_endline "Solution 1:";
  sol01 (sublist 1024 lines) |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_endline;
;;