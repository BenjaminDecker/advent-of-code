let rec checkReport op lastElem  = function
  | [] -> true
  | x::xs -> if op x lastElem && (abs(x-lastElem) <= 3) then
    checkReport op x xs
  else
    false
;;

let startCheckReport = function
  | [] -> true
  | _::[] -> true
  | x::xs::xss -> 
    if x < xs then
      checkReport (>) x (xs::xss)
    else
      checkReport (<) x (xs::xss) 
;;

let rec checkReportDampened op lastElem = function
  | [] -> true
  | x::xs -> if op x lastElem && (abs(x-lastElem) <= 3) then
    checkReportDampened op x xs
  else
    checkReport op lastElem xs
;;

let startCheckReportDampened = function
  | [] -> true
  | _::[] -> true
  | x::xs::xss -> if x < xs then
    checkReportDampened (>) x (xs::xss) || startCheckReport (xs::xss) || startCheckReport (x::xss)
  else
    checkReportDampened (<) x (xs::xss) || startCheckReport (xs::xss) || startCheckReport (x::xss)
;;

let sol01 lines =
  List.map Aoc_2024.Utils.line_to_int_list lines
  |> List.map startCheckReport
  |> List.map Bool.to_int
  |> List.fold_left (+) 0
;;

let sol02 lines =
  List.map Aoc_2024.Utils.line_to_int_list lines
  |> List.map startCheckReportDampened
  |> List.map Bool.to_int
  |> List.fold_left (+) 0
;;


let () = 
  let lines = Aoc_2024.Utils.read_lines "bin/02/input.txt" in
    print_endline "Solution 1:";
    lines
    |> sol01
    |> string_of_int
    |> print_endline;

    print_endline "Solution 2:";
    lines
    |> sol02
    |> string_of_int
    |> print_endline;
;;
