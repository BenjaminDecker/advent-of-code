open Aoc_2024.Utils

type lock_or_key = | Lock | Key;;

let rec count_pins pins =
  let rec count_first_pin = function
    | [] -> 0
    | pins::rest -> (pins |> List.hd |> ((=) '#') |> Bool.to_int) + (count_first_pin rest)
  in
  match List.hd pins with
    | [] -> []
    | _ -> (count_first_pin pins)::(count_pins (pins |> List.map List.tl))
;;

let count_pin_heights lines =
  lines
  |> List.tl
  |> List.map String.to_seq
  |> List.map List.of_seq
  |> count_pins
;;

let parse_lock_or_key lines =
  let (t, lines) = if List.hd lines = "#####" then Lock,lines else Key,List.rev lines in
  (t, count_pin_heights lines)
;;

let is_fit lock key =
  List.for_all2 (fun lock_pin key_pin -> lock_pin + key_pin <= 5) lock key
;;

let rec count_valid_pairs locks keys = 
  match keys with
  | [] -> 0
  | key::keys -> (List.find_all (fun lock -> is_fit lock key) locks |> List.length) + (count_valid_pairs locks keys)
;;

let parse lines = 
  let (locks, keys) = 
    lines
    |> split_list ((=) "")
    |> List.map parse_lock_or_key
    |> List.partition_map (fun lk -> if fst lk = Lock then Left (snd lk) else Right (snd lk)) in
  count_valid_pairs locks keys
;;

let sol01 lines = 
  parse lines
;;

let sol02 _ = 
  0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/25/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;