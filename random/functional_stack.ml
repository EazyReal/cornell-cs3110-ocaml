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
