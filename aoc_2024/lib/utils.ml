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