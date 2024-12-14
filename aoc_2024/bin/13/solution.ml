open Aoc_2024.Utils

let button_of_string s =
  let s = String.split_on_char '+' s in
  let x = List.nth s 1 |> String.split_on_char ',' |> List.hd |> float_of_string |> Q.of_float in
  let y = List.nth s 2 |> float_of_string |> Q.of_float in
  (x,y)
;;

let prize_of_string s =
  let s = String.split_on_char '=' s in
  let x = List.nth s 1 |> String.split_on_char ',' |> List.hd |> float_of_string |> Q.of_float in
  let y = List.nth s 2 |> float_of_string |> Q.of_float in
(x,y)
;;

type block = {
  button_A: Q.t * Q.t;
  button_B: Q.t * Q.t;
  prize: Q.t * Q.t;
};;

let parse_block = function
  | button_A::button_B::prize::_ ->  (
    let button_A = button_of_string button_A in
    let button_B = button_of_string button_B in
    let prize = prize_of_string prize in
    {button_A;button_B;prize}
  )
  | _ -> failwith "Not a valid block"
;;

let get_cost b =
  let det = Q.sub (Q.mul (fst b.button_A) (snd b.button_B)) (Q.mul (fst b.button_B) (snd b.button_A)) in
  if Q.(=) Q.zero det then 0 else
  let det_inv = Q.inv det in
  let x = Q.mul det_inv (Q.sub (Q.mul (snd b.button_B) (fst b.prize)) (Q.mul (fst b.button_B) (snd b.prize))) in
  let y = Q.mul det_inv (Q.sub (Q.mul (fst b.button_A) (snd b.prize)) (Q.mul (snd b.button_A) (fst b.prize))) in
  if (Z.equal (Q.den x) Z.one) && (Z.equal (Q.den y) Z.one) then 3 * (Q.to_int x) + (Q.to_int y) else 0
;;

let rec get_blocks = function
  | ""::rest -> get_blocks rest
  | l1::l2::l3::rest -> (l1::l2::l3::[])::(get_blocks (try (List.tl rest) with _ -> []))
  | _ -> []
;;

let sol01 lines = 
  lines |> get_blocks |> List.map parse_block |> List.map get_cost |> List.fold_left (+) 0
;;

let sol02 lines = 
  lines
  |> get_blocks
  |> List.map parse_block
  |> List.map (fun b -> {
      button_A=b.button_A;
      button_B=b.button_B;
      prize=(Q.add (Q.of_int 10000000000000) (fst b.prize)),(Q.add (Q.of_int 10000000000000) (snd b.prize))
    })
  |> List.map get_cost
  |> List.fold_left (+) 0
;;

let () = 
  Printexc.record_backtrace true;
  let lines = read_lines "bin/13/input.txt" in
  print_endline "Solution 1:";
  sol01 lines |> print_int |> print_newline;
  print_endline "Solution 2:";
  sol02 lines |> print_int |> print_newline;
;;