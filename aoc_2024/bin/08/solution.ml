open Aoc_2024.Utils

let add coords1 coords2 =
  (fst coords1 + fst coords2),(snd coords1 + snd coords2)
;;

let sub coords1 coords2 =
  (fst coords1 - fst coords2),(snd coords1 - snd coords2)
;;

let parseLines lines =
  lines
  |> List.mapi (
    fun y line -> line
    |> String.to_seqi
    |> Seq.filter (fun (_, c) ->  c <> '.')
    |> Seq.map (fun (x, frequency) -> frequency, (x,y))
    |> List.of_seq
  )
  |> List.fold_left (@) []
;;

let getAntinodes antenna1 antenna2 =
  if antenna1 == antenna2 then [] else
  if fst antenna1 <> fst antenna2 then [] else
  let coords1 = snd antenna1 in
  let coords2 = snd antenna2 in
  let diff = sub coords1 coords2 in
  [add coords1 diff; sub coords2 diff]
;;

let isInside width height coords =
  let x = fst coords in let y = snd coords in
  (0 <= x) && (0 <= y) && (x < width) && (y < height)
;;

let rec getAllAntinodes antennas = function
  | [] -> []
  | antenna1::xs -> (antennas |> (List.fold_left (fun antinodes antenna2 -> (getAntinodes antenna1 antenna2)@antinodes) [])) @ (getAllAntinodes antennas xs)
;;

let sol01 lines = 
  let width = lines |> List.hd |> String.length in
  let height = lines |> List.length in
  let antennas = lines |> parseLines in
  (getAllAntinodes antennas antennas)
  |> List.filter (isInside width height)
  |> List.sort_uniq smallerEqual
  |> List.length
;;

let rec getAllAntinodesInLine width height coords diff =
  let newCoords = add coords diff in
  if isInside width height newCoords then newCoords::(getAllAntinodesInLine width height newCoords diff) else []
;;

let getAntinodesPt2 width height antenna1 antenna2 =
  if antenna1 == antenna2 then [] else
  if fst antenna1 <> fst antenna2 then [] else
  let coords1 = snd antenna1 in
  let coords2 = snd antenna2 in
  let diff1 = sub coords1 coords2 in
  let diff2 = sub coords2 coords1 in
  [coords1; coords2] @ (getAllAntinodesInLine width height coords1 diff1) @ (getAllAntinodesInLine width height coords2 diff2)
;;

let rec getAllAntinodesPt2 width height antennas = function
  | [] -> []
  | antenna1::xs -> (antennas |> (List.fold_left (fun antinodes antenna2 -> (getAntinodesPt2 width height antenna1 antenna2)@antinodes) [])) @ (getAllAntinodesPt2 width height antennas xs)
;;

let sol02 lines =
  let width = lines |> List.hd |> String.length in
  let height = lines |> List.length in
  let antennas = lines |> parseLines in
  (getAllAntinodesPt2 width height antennas antennas)
  |> List.sort_uniq smallerEqual
  |> List.length
;;


let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/08/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;