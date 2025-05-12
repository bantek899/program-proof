(* Satisfiability with DPLL algorithm *)

type var = int
type literal = bool * var (* false means negated *)
type clause = literal list
type cnf = clause list

(* 
A cnf [] corresponds to true
A clause [] corresponds to false
*)

exception Not_found         

let rec list_mem a = function
  | [] -> false
  | h :: t -> if a = h then true else list_mem a t

let rec list_map f = function
  | [] -> []
  | h :: t -> f h :: list_map f t 


let rec list_filter f = function
  | [] -> []
  | h :: t -> if f h then h :: list_filter f t  else list_filter f t


let rec subst_cnf (var: var) (bool: bool) (cnf: cnf) =
  let contains_x = list_filter (fun v -> not (list_mem (bool, var) v)) cnf in 
  list_map (fun c -> list_filter (fun v -> v <> (not bool, var)) c) contains_x
  
let rec simple_dpll cnf =
  match cnf with
  | [] -> true
  | c when list_mem [] cnf -> false
  | c ->
     let var = snd (List.hd (List.hd cnf)) in
     simple_dpll(subst_cnf var true c) || simple_dpll(subst_cnf var false c)

let rec unit = function
  | [] -> raise Not_found
  | h :: t ->
     (
       match h with
       | h' :: t' when t' = [] -> h'
       | _ -> unit t
     )
  
let rec pure cnf =  
 

(* Improve on simple_dpll above as follows:
if the formula is empty then it returns true,
if the formula contains the empty clause then it returns false,
if the formula contains a unitary clause then it replaces the corresponding variable by the only possible value,
if the formula contains a pure literal then it replaces the corresponding variable by the value which preserves satisfiability,
otherwise, it splits on an arbitrary variable.
*)

let rec dpll (cnf: cnf) = 
  match cnf with
  | [] -> true
  | c when list_mem [] cnf -> false
  | _ ->
     (*unitary clause *)
     try let u = unit cnf in dpll(subst_cnf (snd u) true cnf) || dpll(subst_cnf (snd u) false cnf)
     with Not_found ->
       try let pl = pure cnf in dpll(subst_cnf (snd pl) (fst pl) cnf)   
       with Not_found ->
         let var = snd (List.hd (List.hd cnf)) in
         dpll(subst_cnf var true cnf) || dpll(subst_cnf var false cnf)
         

(** Parse a CNF file. *)
let parse f : cnf =
  let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s
  in
  let f = load_file f in
  let f = String.map (function '\t' -> ' ' | c -> c) f in
  let f = String.split_on_char '\n' f in
  let f = List.map (String.split_on_char ' ') f in
  let f = List.filter (function "c"::_ | "p"::_ -> false | _ -> true) f in
  let f = List.flatten f in
  let aux (a,c) = function
    | "" -> (a,c)
    | "0" -> (c::a,[])
    | n ->
       let n = int_of_string n in
       let x = if n < 0 then (false,-n) else (true,n) in
       (a,x::c)
  in
  fst (List.fold_left aux ([],[]) f)
         

(* let () = assert (dpll (parse "flat50-1000.cnf")) *)


  
(* let x = true, 0 
let x'= false,0 
let y = true, 1 
let y'= false,1 
let a: cnf = [[x;y]; [x';y]; [x';y']] 
let b = [[x;y]; [x';y]; [x;y']; [x';y']]
 *)
          
(* why does this return true i.e. is satisfiable?
   dpll [[false, 1]; [false, 0]; [false, 1]];; *)  
