(* 
Type inference for small set of programs
Normalization function to reduce program according to type inference
*)

type prog =
  | Bool of bool
  | Int of int
  | Unit of unit
  | Product of prog * prog
  | Add of prog * prog 
  | Lt of prog * prog
  | If of prog * prog * prog
  | Function of (prog list * prog) list
             
type typ =
  | TBool
  | TInt
  | TUnit 
  | TProduct of typ * typ
              
exception Type_error

let rec infer = function
  | Bool _ -> TBool
  | Int _ -> TInt
  | Unit _ -> TUnit
  | Product(a, b) -> TProduct(infer a, infer b)      
  | Add(a, b) ->
     (
       match (infer a, infer b) with
       | (TInt, TInt) -> TInt
       | _ -> raise Type_error
     )
  | Lt(a, b) ->
     (
       match (infer a, infer b) with
       |(TInt, TInt) -> TBool
       | _ -> raise Type_error
     )
  | If(a, b, c) ->
     (
       match (infer a, infer b, infer c) with
       | (TBool, TInt, TInt) -> TInt
       | (TBool, TBool, TBool) -> TBool
       | _ -> raise Type_error
     )
(*  | Function a ->
     (
       match a with
       | [] -> TUnit
       | h :: t -> infer h
     )
 *)
    
let typable p = 
    try
      let _ = infer p in true
    with
    | Type_error -> false
      
let rec reduce = function
  | Bool _ | Int _ | Unit _ -> None
  | Add (Int n1, Int n2) -> Some (Int (n1 + n2))
  | Add (p1, p2) ->
     (
       match reduce p1 with
       | Some p1' -> Some (Add (p1', p2))
       | None ->
          match reduce p2 with
          | Some p2' -> Some (Add (p1, p2'))
          | None -> None
     )
  | Lt (Int n1, Int n2) -> Some (Bool (n1 < n2))
  | Lt (p1, p2) ->
     (
       match reduce p1 with
       | Some p1' -> Some (Lt (p1', p2))
       | None ->
          match reduce p2 with
          | Some p2' -> Some (Lt (p1, p2'))
          | None -> None
     )
  | If (Bool true, p1, p2) -> Some p1
  | If (Bool false, p1, p2) -> Some p2
  | If (p, p1, p2) ->
     (
       match reduce p with
       | Some p' -> Some (If (p', p1, p2))
       | None -> None
     )
  | Product (p1, p2) ->
     (
       match (p1, p2) with
       | (Bool _, Int _) | (Int _, Bool _) -> Some (Product (p1, p2))
       | _ -> 
          (
            match reduce p1 with
            | Some p1' -> Some (Product (p1', p2))
            | None ->
               match reduce p2 with
               | Some p2' -> Some (Product (p1, p2'))
               | None -> None
          )
     )
  
(* Normalize: reduce as much as possible *)
let rec normalize p =
  match reduce p with
  | None -> p
  | Some p' -> normalize p'
                 
                 
                  
