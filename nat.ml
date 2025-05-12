
type nat =
  | Zero
  | Suc of nat


let rec add a b =
  match a with
  | Zero -> b
  | Suc a' -> Suc (add a' b)

let rec even = function
  | Zero -> true
  | Suc Zero -> false
  | Suc (Suc x) -> even x

let pred = function
  | Zero -> None
  | Suc x -> Some x

let rec half = function
  | Zero -> Zero
  | Suc Zero -> Zero
  | Suc (Suc x) -> Suc(half x)

let half_odd x =
  let rec even acc n =
    match n with
    | Zero -> acc
    | Suc Zero -> None
    | Suc (Suc y) ->
       begin match acc with
         Some a -> even (Some (Suc a)) y
       end
  in
  even (Some Zero) x
                   
