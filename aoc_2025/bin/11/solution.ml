open Aoc_2025.Utils

let parse_line line =
  let split = line |> String.split_on_char ':' in
  let device = split |> List.hd in
  let attached = split |> List.tl |> List.hd |> String.split_on_char ' ' |> List.tl in
  (device, attached)
;;

let parse_connections lines =
  lines
  |> List.to_seq
  |> Seq.map parse_line
  |> Hashtbl.of_seq
;;

let solve1 connections =
  let visited = Hashtbl.create 1 in
  let rec imp = function
  | "out" -> 1
  | device -> (
    match (Hashtbl.find_opt visited device) with
    | Some possible_paths -> possible_paths
    | None -> (
      let possible_paths = Hashtbl.find connections device
      |> List.map imp
      |> List.fold_left (+) 0 in
      if (Hashtbl.find_opt visited device |> Option.is_some) then failwith "Uff";
      Hashtbl.add visited device possible_paths;
      possible_paths
    )
  ) in
  imp "you"
;;

let solve2 connections =
  let visited = Hashtbl.create 1 in
  let rec imp = function
  | "out" -> (0, 0, 1)
  | device -> (
    match (Hashtbl.find_opt visited device) with
      | Some path_info -> path_info
      | None -> (
        let (correct_paths, half_correct_paths, incorrect_paths) = Hashtbl.find connections device
        |> List.map imp
        |> List.fold_left (fun (l0, l1, l2) (r0, r1, r2) -> (l0+r0), (l1+r1), (l2+r2)) (0,0,0) in
        let path_info = (
          if device="fft" || device="dac" then (
            (half_correct_paths, incorrect_paths, 0)
          ) else (
            (correct_paths, half_correct_paths, incorrect_paths)
          )
        ) in 
        Hashtbl.add visited device path_info;
        path_info
      )
    ) in let (correct_paths, _, _) = imp "svr" in correct_paths
;;

let sol01 lines =
  lines
  |> parse_connections
  |> solve1
;;

let sol02 lines =
  lines
  |> parse_connections
  |> solve2
;;

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "bin/11/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
