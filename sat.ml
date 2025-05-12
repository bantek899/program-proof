(* Satisfiability *)

type var = int

type formula =
  | Var of var
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | True
  | False


(* Substitution 
A[B/X] replace variable X with formula B in a formula A
*)

let rec subst v b = function
  | Var x when x = v -> b
  | Var x -> Var x                      
  | Not x -> Not(subst v b x)
  | And (x, y) -> And(subst v b x, subst v b y)
  | Or (x, y) -> Or(subst v b x, subst v b y) 
  | True -> True
  | False -> False

(* Free Variables. Return any free var in formula if one exists.
  Here all vars are "free variables"
  *)

let rec free_var = function
  | Var v -> Some v
  | Not v -> free_var v
  | And (f1, f2) | Or (f1, f2) ->
     (
       match free_var f1 with
       | Some v -> Some v
       | None ->
          match free_var f2 with
          | Some v -> Some v
          | None -> None
     )
  | _ -> None


(* Define eval to evaluate closed formulas.
take a formula with no vars and returns a bool
*)

let rec eval = function
  | Var _ -> false
  | And (f1, f2) -> eval f1 && eval f2
  | Or (f1, f2) -> eval f1 || eval f2
  | Not f -> not (eval f)
  | True -> true
  | False -> false


(* Satisfiability. Determine if a formula is satisfiable.
  Does the formula have a truth value assignment that makes it true?
*)

let rec sat f =
  match free_var f with
  | None -> eval f
  | Some v -> sat (subst v True f) || sat (subst v False f)
  


    
