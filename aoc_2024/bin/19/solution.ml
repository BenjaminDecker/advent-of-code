open Aoc_2024.Utils

let parse lines = 
  let (patterns, designs) = split_lines lines in
  let patterns = 
    patterns |> List.hd |> String.split_on_char ',' |> List.map String.trim
  in
  let designs = 
    designs |> List.filter ((<>) "")
  in
  (patterns, designs)
;;

let h = Hashtbl.create 0;;

let rec is_possible patterns = function
  | "" -> 1
  | design -> try Hashtbl.find h design with _ -> let n = patterns |> List.map (fun pattern -> 
      if not (String.starts_with ~prefix:pattern design) then 0 else
        let design_length = String.length design in 
        let pattern_length = String.length pattern in
        is_possible patterns (String.sub design (pattern_length) (design_length - pattern_length))
      
    ) in
    let n = List.fold_left (+) 0 n in
    Hashtbl.add h design n;
    n
;;

let sol01 lines = 
  let (patterns, designs) = parse lines in
  designs
  |> List.map (is_possible patterns)
  |> List.filter ((<>) 0)
  |> List.length
;;

let sol02 lines = 
  let (patterns, designs) = parse lines in
  designs
  |> List.map (is_possible patterns)
  |> List.fold_left (+) 0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/19/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;