open Aoc_2024.Utils

let line_to_char_array line =
  line |> String.to_seq |> List.of_seq
;;

let lines_to_char_matrix lines =
  lines |> List.map line_to_char_array
;;

type direction = N | E | S | W | NE | SE | SW | NW

(* let straights = [N;E;S;W] *)

let diagonals = [NE;SE;SW;NW]

let nextCoords coords = function
  | N -> fst coords, snd coords - 1
  | E -> fst coords + 1, snd coords
  | S -> fst coords, snd coords + 1 
  | W -> fst coords - 1, snd coords
  | NE -> fst coords + 1, snd coords - 1
  | SE -> fst coords + 1, snd coords + 1 
  | SW -> fst coords - 1, snd coords + 1
  | NW -> fst coords - 1, snd coords - 1
;;

let rec extractCharList coords charMatrix d =
  if (fst coords < 0) || (snd coords < 0) then [] else
  match List.nth_opt charMatrix (snd coords) with
  | None -> [] 
  | Some row -> match List.nth_opt row (fst coords) with
    | None -> []
    | Some c -> c::(extractCharList (nextCoords coords d) charMatrix d)
;;

let countXMAS charMatrix directions coords =
  List.map (fun coord -> directions
    |> List.map (extractCharList coord charMatrix)
    |> List.map List.to_seq
    |> List.map String.of_seq
    |> List.map (countSubstrOccurences "SAMX")
    |> List.fold_left (+) 0
  ) coords
  |> List.fold_left (+) 0
;;

let sol01 charMatrix = 
  let height = List.length charMatrix in
  let width = List.length (List.hd charMatrix) in

  let xRange = range 0 width in
  let yRange = range 0 height in

  let corners = [(0,0);(0,height-1);(width-1, 0);(width-1, height-1)] in
  let topRow = xRange |> List.map (fun x-> (x,0)) in
  let bottomRow = xRange |> List.map (fun x-> (x,height-1)) in
  let leftSide = yRange |> List.map (fun y-> (0, y)) in
  let rightSide = yRange |> List.map (fun y-> (width-1, y)) in

  let topDown = countXMAS charMatrix [S] topRow in
  let bottomUp = countXMAS charMatrix [N] bottomRow in
  let leftToRight = countXMAS charMatrix [E] leftSide in
  let rightToLeft = countXMAS charMatrix [W] rightSide in

  let topDiagonals = countXMAS charMatrix diagonals topRow in
  let bottomDiagonals = countXMAS charMatrix diagonals bottomRow in
  let leftDiagonals = countXMAS charMatrix diagonals leftSide in
  let rightDiagonals = countXMAS charMatrix diagonals rightSide in

  let cornerDiagonals = countXMAS charMatrix diagonals corners in

  topDown + bottomUp + leftToRight + rightToLeft + topDiagonals + bottomDiagonals + leftDiagonals + rightDiagonals - cornerDiagonals
;;

let getElem matrix coord =
  if (fst coord < 0) || (snd coord < 0) then None else
    match List.nth_opt matrix (snd coord) with
    | None -> None
    | Some row -> List.nth_opt row (fst coord)
;;

let checkIfMas charMatrix coord =
  let center = getElem charMatrix coord |> unwrap in
  let topRight = getElem charMatrix (nextCoords coord  NE) |> unwrap in
  let bottomRight = getElem charMatrix (nextCoords coord  SE) |> unwrap in
  let bottomLeft = getElem charMatrix (nextCoords coord  SW) |> unwrap in
  let topLeft = getElem charMatrix (nextCoords coord  NW) |> unwrap in
    center == 'A'
    && ((topRight == 'M' && bottomLeft == 'S') || (topRight == 'S' && bottomLeft == 'M'))
    && ((topLeft == 'M' && bottomRight == 'S') || (topLeft == 'S' && bottomRight == 'M')) 

let sol02 charMatrix =
  let height = List.length charMatrix in
  let width = List.length (List.hd charMatrix) in

  let xRange = range 1 (width - 1) in
  let yRange = range 1 (height - 1) in 

  List.map (fun y -> List.map (fun x -> Bool.to_int (checkIfMas charMatrix (x,y))) xRange) yRange
  |> List.map (List.fold_left (+) 0)
  |> List.fold_left (+) 0
;;
  
let () = 
  (* Printexc.record_backtrace true;; *)
  let lines = read_lines "bin/04/input.txt" in
  let charMatrix = lines_to_char_matrix lines in
  print_endline "Solution 1:";
  print_int (sol01 charMatrix);
  print_newline ();
  print_endline "Solution 2:";
  print_int (sol02 charMatrix);
  print_newline ();
;;
