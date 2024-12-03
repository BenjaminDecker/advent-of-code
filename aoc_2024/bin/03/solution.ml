let rec getResult regex line =
  try let idx = Str.search_forward regex line 0 in
  let result = int_of_string (Str.matched_group 1 line) * int_of_string (Str.matched_group 2 line) in
  result + getResult regex (Str.string_after line (idx+8))
with Not_found -> 0 

let extractEnabledSubstrings s =
  let regexDont = Str.regexp_string "don't()" in
  let regexDo = Str.regexp_string "do()" in
  Str.split regexDo s
  |> List.map (Str.split regexDont)
  |> List.map List.hd


let () = 
let lines = Aoc_2024.Utils.read_lines "bin/03/input.txt" in
let regex = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|} in

print_endline "Solution 1:";
lines
 |> List.map (getResult regex)
 |> List.fold_left (+) 0
 |> print_int
 |> print_newline;

print_endline "Solution 2:";
lines
|> String.concat ""
|> extractEnabledSubstrings
|> List.map (getResult regex)
|> List.fold_left (+) 0
|> print_int
|> print_newline;
;;
