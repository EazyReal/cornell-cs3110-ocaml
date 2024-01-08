let print_list xs = List.iter (fun x -> print_int x; print_string " ") xs; print_newline ()

(* Exercise: patterns [★★★]

Using pattern matching, write three functions, one for each of the following properties. Your functions should return true if the input list has the property and false otherwise.

the list’s first element is "bigred"

the list has exactly two or four elements; do not use the length function

the first two elements of the list are equal *)
let ep xs = match xs with
  | x::xs when x == "bigred" -> true
  | x::y::[] -> true
  | x::y::z::w::[] -> true
  | x::y::xs when x == y -> true
  | _ -> false

  (* Exercise: library [★★★]

  Consult the List standard library to solve these exercises:
  
  Write a function that takes an int list and returns the fifth element of that list, if such an element exists. If the list has fewer than five elements, return 0. Hint: List.length and List.nth.
  
  Write a function that takes an int list and returns the list sorted in descending order. Hint: List.sort with Stdlib.compare as its first argument, and List.rev. *)
let fifth xs =
  if List.length xs >= 5 then List.nth xs 4
  else 0

let sortrev xs = List.sort compare xs |> List.rev

(* Exercise: library puzzle [★★★]

Write a function that returns the last element of a list. Your function may assume that the list is non-empty. Hint: Use two library functions, and do not write any pattern matching code of your own.

Write a function any_zeroes : int list -> bool that returns true if and only if the input list contains at least one 0. Hint: use one library function, and do not write any pattern matching code of your own.

Your solutions will be only one or two lines of code each. *)
let last xs = List.nth xs (List.length xs - 1) 
let last2 xs = List.hd @@ List.rev xs
let any_zero xs = List.exists (fun x -> x == 0) xs

(* 
Exercise: take drop [★★★]

Write a function take : int -> 'a list -> 'a list such that take n lst returns the first n elements of lst. If lst has fewer than n elements, return all of them.

Write a function drop : int -> 'a list -> 'a list such that drop n lst returns all but the first n elements of lst. If lst has fewer than n elements, return the empty list. *)

(* Exercise: take drop tail [★★★★]

Revise your solutions for take and drop to be tail recursive, if they aren’t already. Test them on long lists with large values of n to see whether they run out of stack space. To construct long lists, use the -- operator from the lists section. *)

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


(* Exercise: unimodal [★★★]

Write a function is_unimodal : int list -> bool that takes an integer list and returns whether that list is unimodal. A unimodal list is a list that monotonically increases to some maximum value then monotonically decreases after that value. Either or both segments (increasing or decreasing) may be empty. A constant list is unimodal, as is the empty list. *)

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
