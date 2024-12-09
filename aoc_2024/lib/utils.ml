let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let line_to_int_list line =
  String.split_on_char ' ' line |> List.filter (fun x -> x <> "") |> List.map int_of_string
;;

let lines_to_int_list_list lines = 
  List.map line_to_int_list lines
;;

let range a b =
  let rec imp l i =
    if i < a then l else imp (i::l) (i-1) in
  imp [] (b-1)
;;

let lines_to_char_matrix lines =
  let width = String.length (List.hd lines) in
  let height = List.length lines in
  let chars = Array.make_matrix width height '.' in
  lines
  |> List.iteri (
    fun y row -> (List.iteri (
      fun x elem -> chars.(x).(y) <- elem
    ) (row |> String.to_seq |> List.of_seq))
  );
  chars
;;

let printCharMatrix chars =
  let width = Array.length chars in
  let height = Array.length (chars.(0)) in
  List.iter (
    fun y -> List.iter (
      fun x -> chars.(x).(y) |> print_char
    ) (range 0 (width)); print_newline ();
  ) (range 0 (height))
;;

let printFlatCharArray width arr = 
  Array.iteri (fun idx elem -> if idx mod width == 0 then print_newline (); elem |> print_char;) arr

let rec countSubstrOccurences substr str =
  let lengthSubstr = String.length substr in
  let lengthStr = String.length str in
  try
  if String.sub str 0 lengthSubstr = substr then
    1 + countSubstrOccurences substr (String.sub str 1 (lengthStr-1))
  else
    countSubstrOccurences substr (String.sub str 1 (lengthStr-1))
  with Invalid_argument _ -> 0
;;

let unwrap o =
  match o with
  | Some x -> x
  | None -> raise (Failure "Unwrap failed")
;;

type coords = {
  x:int;
  y:int;
}

let coords_of_tuple t = 
  {x=fst t;y=snd t}
;;

let printCoords coords = 
  print_string "(";
  print_int coords.x;
  print_string ",";
  print_int coords.y;
  print_endline ")";
;;

type direction = N | E | S | W | NE | SE | SW | NW

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

let rec getLast = function
  | [] -> raise (Failure "Empty list")
  | x::[] -> x
  | _::xs -> getLast xs
;;

let rec removeLast = function
  | [] -> raise (Failure "Empty list")
  | _::[] -> []
  | x::xs -> x::(removeLast xs)
;;

let rec popLast = function
  | [] -> raise (Failure "Empty list")
  | x::[] -> (x, [])
  | x::xs -> let (x', xs') = popLast xs in (x', x::xs')
;;