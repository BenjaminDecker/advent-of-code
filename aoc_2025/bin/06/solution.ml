open Aoc_2025.Utils

let op_of_char_opt = function
  | '+' -> Some (+)
  | '*' -> Some ( * )
  | _ -> None
;;

let neutral_element_of_op_char_opt = function
  | '+' -> Some 0
  | '*' -> Some 1
  | _ -> None
;;

let sol01 lines =
  let nlines = lines |> List.find_index (fun s -> (String.contains s '+') || (String.contains s '*')) |> Option.get in
  let numbers =
    lines
    |> List.take nlines
    |> List.map (String.split_on_char ' ')
    |> List.map (List.filter (fun s -> (s |> String.length) > 0))
    |> List.map (List.map int_of_string)
  in
  let ops = List.nth lines nlines |> String.to_seq |> Seq.filter_map op_of_char_opt |> List.of_seq in

  List.combine (transpose_list_list numbers) ops
  |> List.map (
    fun (operands, op) -> (
      let acc = operands |> List.hd in
      let operands = operands |> List.tl in
      operands |> List.fold_left op acc
    )
  )
  |> List.fold_left (+) 0
;;

let do_calculation cols =
  let nlines, op_char = cols |> List.hd |> find_with_index_opt (fun c -> (c = '+') || (c = '*')) |> Option.get in
  let op = op_char |> op_of_char_opt |> Option.get in
  let neutral_element = op_char |> neutral_element_of_op_char_opt |> Option.get in

  cols
  |> List.map (List.take nlines)
  |> List.map List.to_seq
  |> List.map String.of_seq
  |> List.map String.trim
  |> List.map int_of_string
  |> List.fold_left op neutral_element
;;

let cols lines =
  lines
  |> List.map String.to_seq
  |> List.map List.of_seq
  |> transpose_list_list
  |> split_list (List.for_all (fun c -> c=' '))
;;

let sol02 lines =
  lines
  |> cols
  |> List.map do_calculation
  |> List.fold_left (+) 0
;;

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "bin/06/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;
