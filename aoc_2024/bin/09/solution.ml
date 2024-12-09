open Aoc_2024.Utils

type fileSystemBlock = 
  | Data of int * int
  | Free of int
;;

let rec getBlocks id = function
  | [] -> []
  | dataLength::[] -> [Data (dataLength,id)]
  | dataLength::freeLength::xs -> (Data (dataLength, id))::(Free freeLength)::(getBlocks (id+1) xs)
;;

let parseLine line =
  line |> String.to_seq |> Seq.map int_of_char |> Seq.map (fun ascii -> ascii - (int_of_char '0')) |> List.of_seq |> (getBlocks 0)
;;

let eulerSum lower upper  =
  ((upper * (upper - 1)) / 2) - (max 0 (((lower-1) * lower) / 2))
;;

let rec calculateChecksum position blocksLeft blocksRight =
  let (left, restLeft) = (List.hd blocksLeft, List.tl blocksLeft) in
  let (right, restRight) = (List.hd blocksRight, List.tl blocksRight) in
  match (left, right) with 
  | Data (length, id), Data (length', id') -> if id==id' then
      id' * eulerSum position (position + length')
    else 
      id * (eulerSum position (position + length)) + (calculateChecksum (position + length) restLeft blocksRight)
  | Data (length, id), Free _ -> id * (eulerSum position (position + length)) + (calculateChecksum (position + length) restLeft blocksRight)
  | Free length, Data (length', id') -> if length >= length' then  
      id' * (eulerSum position (position + length')) + (calculateChecksum (position+length') ((Free (length - length'))::restLeft) restRight)
    else
      id' * (eulerSum position (position + length)) + (calculateChecksum (position+length) restLeft ((Data (length' - length, id'))::restRight))
  | Free _, Free _ -> calculateChecksum position blocksLeft restRight
;;

let string_of_block = function
  | Data (length, id) -> "Data(" ^ string_of_int length ^ ", " ^ string_of_int id ^ ")"
  | Free length -> "Free(" ^ string_of_int length ^ ")"
;;

let sol01 lines = 
  let line = lines |> List.hd in
  let blocks = line |> parseLine in
  (* blocks |> List.map string_of_block |> List.iter print_endline; *)
  calculateChecksum 0 blocks (List.rev blocks)
;;

let rec insertData (length, id) = function
  | [] -> None
  | (Free length')::rest -> if length <= length' then 
      let newBlocks = (Free 0)::(Data (length, id))::(Free (length'-length))::rest in
      Some newBlocks
    else
      (match insertData (length, id) rest with
    | None -> None
    | Some tail -> Some ((Free length')::tail))
  | d::rest -> (match insertData (length, id) rest with
    | None -> None
    | Some tail -> Some (d::tail))
;;

let unwrapData = function
  | Free _ -> raise (Failure "Unwrap failed")
  | Data (length, id) -> (length, id)
;;

let unwrapFree = function
  | Data _ -> raise (Failure "Unwrap failed")
  | Free l -> l
;;

let rec shiftBlocks = function
  | data::[] -> [data]
  | blocks ->
  let (dataTuple, newBlocks) = popLast blocks in
  let dataTuple = unwrapData dataTuple in

  match insertData dataTuple newBlocks with
  | None -> let (free, newBlocks') = popLast newBlocks in (shiftBlocks newBlocks') @ [free; (Data (fst dataTuple, snd dataTuple))]
  | Some newBlocks' -> let (f, newBlocks'') = popLast newBlocks' in let f = unwrapFree f in (shiftBlocks newBlocks'' ) @ [Free (f+(fst dataTuple))]
;;

let rec calculateChecksum2 position = function
  | [] -> 0
  | (Free length)::rest -> calculateChecksum2 (position + length) rest
  | (Data (length, id))::rest -> id * (eulerSum position (position+length)) + calculateChecksum2 (position + length) rest 
;;

let sol02 lines = 
  let line = lines |> List.hd in
  let blocks = line |> parseLine in
  let blocks = blocks |> shiftBlocks in
  blocks |> (calculateChecksum2 0)
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/09/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;