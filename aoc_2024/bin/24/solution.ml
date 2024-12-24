open Aoc_2024.Utils

type gate_type = | AND | OR | XOR;;

let gate_type_of_string = function
  | "AND" -> AND
  | "OR" -> OR
  | "XOR" -> XOR
  | _ -> failwith "Not a valid gate"
;;

type gate = {
  input:(string * string);
  op:gate_type;
  output:string;
}

module Gate = struct
  type t = gate
  let compare lhs rhs = compare (lhs.output) (rhs.output)
end

module GateSet = Set.Make(Gate);;

module Wire = struct 
  type t = string * bool
  let compare lhs rhs = compare (fst lhs) (fst rhs)
end

module WireSet = Set.Make(Wire);;

let parse_wire line =
  line
  |> (String.split_on_char ':')
  |> (fun s -> (s |> List.hd),(s |> List.tl |> List.hd |> String.exists ((=) '1')))
;;

let parse_gate line =
  line
  |> (String.split_on_char ' ')
  |> (fun s -> {input=(List.nth s 0,List.nth s 2);op=(List.nth s 1 |> gate_type_of_string);output=List.nth s 4})
;;

let parse lines = 
  let (initial_wires, gates) = split_lines lines in
  let wires = 
  initial_wires
  |> List.map parse_wire
  |> WireSet.of_list
  in
  let gates =
  gates
  |> List.map parse_gate
  |> GateSet.of_list
  in
  (wires,gates)
;;

let evaluate lhs rhs = function
  | AND -> lhs && rhs
  | OR -> lhs || rhs
  | XOR -> lhs <> rhs
;;

let rec get_wire_values wires gates wire =
  if WireSet.exists (fun w -> (fst w) = wire) wires then wires else (
  let gate = GateSet.find_first (fun g -> (g.output >= wire)) gates in
  let wires = get_wire_values wires gates (fst gate.input) in
  let wires = get_wire_values wires gates (snd gate.input) in
  let lhs = WireSet.find_first (fun w -> (fst w) >= (fst gate.input)) wires in
  let rhs = WireSet.find_first (fun w -> (fst w) >= (snd gate.input)) wires in
  let result = evaluate (snd lhs) (snd rhs) gate.op in
  WireSet.add (wire,result) wires
  )
;;

let get_factor label =
  String.sub label 1 2 |> int_of_string |> Int.shift_left 1
;;

let sol01 lines = 
  let (wires,gates) = parse lines in
  gates
  |> GateSet.filter (fun g -> g.output.[0] = 'z')
  |> (fun g -> GateSet.fold (fun g acc -> get_wire_values acc gates g.output) g wires)
  |> WireSet.filter (fun w -> (fst w).[0] = 'z')
  |> WireSet.to_seq
  |> Seq.map (fun w -> (get_factor (fst w)) * (Bool.to_int (snd w)))
  |> Seq.fold_left (+) 0
;;

let sol02 _ = 
  0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/24/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;