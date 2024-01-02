let print_list xs = List.iter (fun x -> print_int x; print_string " ") xs; print_newline ()

let ep xs = match xs with
  | x::xs when x == "bigred" -> true
  | x::y::[] -> true
  | x::y::z::w::[] -> true
  | x::y::xs when x == y -> true
  | _ -> false


let fifth xs =
  if List.length xs >= 5 then List.nth xs 4
  else 0

let sortrev xs = List.sort compare xs |> List.rev

let last xs = List.nth xs (List.length xs - 1) 

let any_zero xs = List.exists (fun x -> x == 0) xs

let rec take_tail acc xs n = match xs with 
  | [] -> acc
  | x::xs when n > 0 -> take_tail (acc@[x]) xs (n-1) 
  | _ -> acc

let take = take_tail []

let rec drop xs n = match xs with
  | [] -> []
  | x::xs when n > 0 -> drop xs (n-1)
  | xs -> xs

(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
 let rec from i j l =
  if i > j then l
  else from i (j - 1) (j :: l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *) 
let (--) i j =
  from i j []

let longlen = 10
let longlist = 0 -- longlen

let is_unimodal xs = 
  let rec aux xs afterPeak = match xs with
    | [] -> true
    | x::[] -> true
    | x::y::xs ->
      if afterPeak then
        if x >= y then aux (y::xs) true
        else false
      else (
        if x > y then aux (y::xs) true
        else aux (y::xs) false
      )
  in aux xs false

(* test the four cases *)
let () = 
  print_endline (string_of_bool (ep ["bigred"; "ad"; "b"]));
  print_endline (string_of_bool (ep ["a"; "b"]));
  print_endline (string_of_bool (ep ["a"; "b"; "c"; "d"]));
  print_endline (string_of_bool (ep ["a"; "a"; "b"]));
  print_endline (string_of_bool (ep ["a"; "b"; "c"]));
  print_int (fifth [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]); print_endline "";
  print_endline (string_of_bool (any_zero [0; 1; 2; 3; 4; 5]));
  print_list (sortrev [1; 2; 3; 4; 5]);
  print_list (take longlist 10);
  print_list (drop longlist (longlen-1));
  print_endline (string_of_bool (is_unimodal [0; 1; 2; 3; 4; 5]));
  print_endline (string_of_bool (is_unimodal @@ List.rev[0; 1; 2; 3; 4; 5]));
  print_endline (string_of_bool (is_unimodal [0; 1; 2; 3; 2; 1]));
  print_endline (string_of_bool (is_unimodal [0; 1; 2; 3; 2; 3]));

open Option

let (>>|) opt f = match opt with
| Some x -> Some (f x)
| None -> None

module MyStack = struct
  type 'a t = 'a list
  let create () = []
  let push x s = x::s
  let pop s = match s with
    | [] -> None
    | x::xs -> Some (x, xs)
end

let q = 
  let open MyStack in
  create () |> push 1 |> pop

