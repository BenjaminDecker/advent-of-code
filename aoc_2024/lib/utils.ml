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
  lines |> List.iteri (
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

type direction = N | E | S | W | NE | SE | SW | NW

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

type flat_char_array = {
  width: int;
  height: int;
  chars: char array;
};;

let flat_char_array_of_lines lines = 
  let width = String.length (List.hd lines) in
  let height = List.length lines in 
  let chars = Array.make (width * height) '.' in
  lines |> List.iteri (
    fun y row -> (List.iteri (
      fun x elem -> chars.(y * width + x) <- elem
    ) (row |> String.to_seq |> List.of_seq))
  );
  {width=width; height=height; chars=chars}
;;

let print_flat_char_array width arr = 
  Array.iteri (fun idx elem -> if idx mod width == 0 then print_newline (); elem |> print_char;) arr

let string_of_coord coord =
  "(" ^ string_of_int (fst coord) ^ ", " ^ string_of_int (snd coord) ^ ")"
;;

let next_coord (x, y) = function
  | N -> (x, y - 1)
  | E -> (x + 1, y)
  | S -> (x, y + 1)
  | W -> (x - 1, y)
  | NE -> (x + 1, y - 1)
  | SE -> (x + 1, y + 1)
  | SW -> (x - 1, y + 1)
  | NW -> (x - 1, y - 1)
;;

class ['a] flat_array (lines: 'a list list) =
object (self)
  val width = List.length (List.hd lines)
  val height = List.length lines
  val mutable arr = (
    let width = List.length (List.hd lines) in
    let height = List.length lines in
    Array.init (width * height) (
      fun idx -> let x = idx mod width in let y = idx / width in (List.nth (List.nth lines y) x)
    )
  )

  method width = 
    width

  method height = 
    height

  method is_valid coord =
    let x = (fst coord) in let y = (snd coord) in
    0 <= x && 0 <= y && x < width && y < height

  method idx_of_coord coord =
    if not (self#is_valid coord) then raise (Failure "Not a valid index") else 
    width * (snd coord) + (fst coord)

  method coord_of_idx idx =
    let coord = idx mod width, idx / width in
    if not (self#is_valid coord) then raise (Failure "Not a valid index") else 
    coord

  method get_at coord = 
    if not (self#is_valid coord) then raise (Failure "Not a valid index") else 
    arr.(self#idx_of_coord coord)

  method put_at coord elem = 
    if not (self#is_valid coord) then raise (Failure "Not a valid index") else 
    arr.(self#idx_of_coord coord) <- elem;

  method get_neighbor_coords directions coord =
    directions |> List.map (next_coord coord) |> List.filter (self#is_valid) 

  method find_first f =
    arr |> Array.find_index f |> Option.get |> self#coord_of_idx

  method find_all_coords f =
    arr |> Array.to_seqi |> Seq.filter_map (fun (idx, elem) -> if (f elem) then (Some (self#coord_of_idx idx)) else None)

  method print f =
    arr |> Array.iteri (fun idx elem -> if idx mod width == 0 then print_newline (); elem |> f |> print_string;) |> print_newline

end;;

let smallerEqual coord1 coord2 = 
  let comp = compare (snd coord1) (snd coord2) in
  if comp <> 0 then comp else compare (fst coord1) (fst coord2)
;;

let add_coords coord1 coord2 =
  (fst coord1 + fst coord2),(snd coord1 + snd coord2)
;;

let rec numDigits = function
  | 0 -> 0
  | n -> 1 + numDigits (n/10)
;;

let rec pow b e =
  match e with 
  | 0 -> 1
  | 1 -> b
  | e -> b * pow b (e-1)
;;

let string_of_list f = function
  | [] -> "[]"
  | x::xs -> 
    let rec do_it = function
    | [] -> "]"
    | x::xs -> "," ^ f x ^ (do_it xs)
    in
    "[" ^ f x ^ do_it xs
;;

let just_print f e =
  e |> f |> print_endline; e
;;

let coord_of_string s = 
  let s = String.split_on_char ',' s in
  (List.nth s 0 |> int_of_string, List.nth s 1 |> int_of_string)
;;

let rec split_lines = function
  | ""::rest -> ([], rest)
  | line::rest -> let (lhs,rhs) = split_lines rest in (line::lhs, rhs)
  | [] -> failwith "Empty lists are not allowed"
;;

let rec sublist length list = 
  match (length, list) with
  | (0,_) -> []
  | (n,x::xs) -> x::(sublist (n-1) xs)
  | _ -> failwith "List too short"
;;

module Coord = struct 
  type t = int * int
  let compare = compare
end

module CoordSet = Set.Make(Coord)

let range_seq a b =
  Seq.unfold (fun a -> if a >= b then None else Some (a,a+1)) a
;;

let sub lhs rhs = 
  ((fst lhs - fst rhs), (snd lhs - snd rhs))
;;

let rec split_list f = function
  | [] -> [[]]
  | x::xs -> match split_list f xs with
    | []::xs' -> if f x then []::[]::xs' else [x]::xs'
    | x'::xs' -> if f x then []::x'::xs' else (x::x')::xs'
    | [] -> failwith "Impossible"
;;