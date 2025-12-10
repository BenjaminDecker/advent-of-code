open Aoc_2025.Utils

let int_of_lights s =
  s
  |> String.to_seq
  |> List.of_seq
  |> List.rev
  |> List.fold_left (fun acc c -> acc*2 + if c='#' then 1 else 0) 0
;;

let remove_brackets s = String.sub s 1 ((s |> String.length)-2);;

let parse_ints s = s |> remove_brackets |> String.split_on_char ',' |> List.map int_of_string;;

let parse_lights lights_str =
  lights_str
  |> remove_brackets
  |> int_of_lights
;;

let parse_button button_str =
  button_str
  |> remove_brackets
  |> String.split_on_char ','
  |> List.map int_of_string
  |> List.map (Int.shift_left 1)
  |> List.fold_left (+) 0
;;

let parse_joltages s = s |> parse_ints;;

let parse_line line =
  let splits = line
  |> String.split_on_char ' '
  in
  let lights = splits |> List.hd |> parse_lights in
  let buttons, joltages = splits |> List.tl |> List.partition (fun s -> String.contains s '(') in
  (lights, (buttons |> List.map parse_button), joltages |> List.hd |> parse_joltages)
;;

let solve line =
  let target_config, buttons, _ = line |> parse_line in
  let rec imp light_configs =
    match Hashtbl.find_opt light_configs target_config with
    | Some n -> n
    | None -> (
      light_configs
      |> Hashtbl.to_seq
      |> List.of_seq
      |> Seq.unfold (
        function
        | [] -> None
        | (k1,v1)::xs ->
          let s = xs
          |> List.to_seq
          |> Seq.map (fun (k2,v2) -> ((Int.logxor k1 k2), (v1+v2))) in
          Some(s, xs)
      )
      |> Seq.concat
      |> Seq.iter (
        fun (k,v) -> (
          match Hashtbl.find_opt light_configs k with
          | None -> Hashtbl.add light_configs k v;
          | Some v' -> Hashtbl.replace light_configs k (min v v')
        ));
      imp light_configs
    )
  in
  let light_configs = Hashtbl.create (buttons |> List.length) in
  buttons |> List.iter (
    fun b -> Hashtbl.add light_configs b 1
  );
  imp light_configs
;;

let sol01 lines =
  lines
  |> List.map solve
  |> List.fold_left (+) 0
;;

let sol02 lines =
  lines
  |> List.length
;;

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "bin/10/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
