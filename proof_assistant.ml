let () = Printexc.record_backtrace true
       
type tvar = string
type var = string

type ty =
  | TVar of tvar
  | Impl of ty * ty

type tm =
  | Var of var
  | App of tm * tm
  | Abs of var * ty * tm

type context = (var * ty) list

exception Type_error
         
let rec string_of_ty = function
  | TVar x -> x
  | Impl (a, b) -> "(" ^ string_of_ty a ^ " => " ^ string_of_ty b ^ ")"

let rec string_of_tm = function
  | Var x -> x
  | App (t, u) -> "(" ^ string_of_tm t ^ " " ^ string_of_tm u ^ ")"
  | Abs (x, a, t) -> "(fun (" ^ x ^ " : " ^ string_of_ty a ^ ") -> " ^ string_of_tm t ^ ")"

let rec infer_type context = function
  | Var x -> (try List.assoc x context with | Not_found -> raise Type_error)
  | App (t, u) ->
     (
       match infer_type context t with
       | Impl (a, b) -> check_type context u a; b
       | _ -> raise Type_error
     )             
  | Abs (x, a, t) -> Impl (a, infer_type ((x, a) :: context) t)
                       
and check_type context term typ =
  if infer_type context term <> typ then raise Type_error

                                           
