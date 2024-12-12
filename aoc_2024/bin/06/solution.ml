open Aoc_2024.Utils

let rotate = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N
  | _ -> raise (Failure "diagonal directions are not allowed")
;;

type coords = {
  x:int;
  y:int;
}

let nextCoords coords = function
  | N -> {x=coords.x; y=coords.y - 1}
  | E -> {x=coords.x + 1; y= coords.y}
  | S -> {x=coords.x; y= coords.y + 1 }
  | W -> {x=coords.x - 1; y= coords.y}
  | NE -> {x=coords.x + 1; y= coords.y - 1}
  | SE -> {x=coords.x + 1; y= coords.y + 1} 
  | SW -> {x=coords.x - 1; y= coords.y + 1}
  | NW -> {x=coords.x - 1; y= coords.y - 1}
;;

type guard = {
  pos:coords;
  dir:direction;
};;

type tile = 
  | Free of direction list
  | Obstacle
;;

let idxOfCoords width coords = 
  width * coords.y + coords.x
;;

let coordsOfIdx width idx = 
  {x=idx mod width; y=idx/width}
;;

let charsOfLines lines =
  let width = String.length (List.hd lines) in
  let height = List.length lines in
  Array.init (width * height) (fun idx -> let coords = coordsOfIdx width idx in String.get (List.nth lines coords.y) coords.x)
;;

let tileOfChar = function
  | '.' -> Free []
  | '#' -> Obstacle
  | '^' -> Free [N]
  | _ -> raise (Failure "Not a valid tile char")
;;

let tilesOfChars arr = 
  arr |> Array.map tileOfChar
;;

let findGuard lines = 
  let y = unwrap (lines |> List.find_index (String.exists ((=)'^'))) in
  let x = String.index (List.nth lines y) '^' in
  {pos={x=x;y=y};dir=N}
;;

type lab = {
  width:int;
  map:tile array;
};;

let rec nextGuard lab guard = 
  let nextCoords = nextCoords guard.pos guard.dir in
  if lab.width <= nextCoords.x || nextCoords.x < 0 then None else
  try
    match lab.map.(idxOfCoords lab.width nextCoords) with
    | Obstacle -> nextGuard lab {pos=guard.pos; dir=rotate guard.dir}
    | _ -> Some {pos=nextCoords; dir=guard.dir}
  with Invalid_argument _ -> None 
;;

type result = 
  | Normal of lab * guard
  | Outside
  | Loop 
;;

let step lab guard =
  match lab.map.(idxOfCoords lab.width guard.pos) with
  | Free visited -> lab.map.(idxOfCoords lab.width guard.pos) <- Free (guard.dir::visited);
  | _ -> ();
  ;
  match nextGuard lab guard with
  | None -> Outside
  | Some guard -> match lab.map.(idxOfCoords lab.width guard.pos) with
    | Free visited -> if (List.mem guard.dir visited) then Loop else Normal (lab,guard)
    | _ -> raise (Failure "Uff")
;;

let rec checkPath lab guard =
  match step lab guard with
  | Normal (lab,guard) -> checkPath lab guard
  | Outside -> Outside
  | Loop -> Loop
;;

let countTile = function
  | Free visited -> if (List.length visited) > 0 then 1 else 0
  | _ -> 0
;;

let countVisitedTiles lab =
  lab.map|> Array.map countTile |> Array.fold_left (+) 0
;;

let sol01 lines = 
  let width = String.length (List.hd lines) in
  let guard = findGuard lines in
  let tiles = lines |> charsOfLines |> tilesOfChars in
  let lab = {width=width; map=tiles} in

  match checkPath lab guard with
  | Outside -> countVisitedTiles lab
  | _ -> 0
;;

let putObstacle lab coord =
  lab.map.(idxOfCoords lab.width coord) <- Obstacle
;;

let sol02 lines = 
  let width = String.length (List.hd lines) in
  let guard = findGuard lines in
  let tiles = lines |> charsOfLines |> tilesOfChars in
  let lab = {width=width; map=tiles} in
  let mapBackup = tiles |> Array.copy in

  let _ = checkPath lab guard in
  let visitedCoords = lab.map |> Array.to_list |> List.mapi ( fun idx elem ->
    match elem with
    | Free [] -> None
    | Free _ -> Some (coordsOfIdx width idx)
    | _ -> None
  ) |> List.filter Option.is_some |> List.map Option.get in

  visitedCoords 
  |> List.map (fun coord -> let lab = {width=lab.width;map=(Array.copy mapBackup)} in putObstacle lab coord; checkPath lab guard)
  |> List.filter ((=) Loop)
  |> List.length
;;


let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/06/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;