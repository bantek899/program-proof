type var = int
type literal = bool * var (* false means negated *)
type clause = literal list
type cnf = clause list

let var i j n : var =
  let i = i mod 9 in
  let j = j mod 9 in
  9*(9*i+j)+n

let square i j = List.init 3 (fun i'' ->
                     (List.init 3 (fun i' ->
                          i, j + i')))
                                      
let sudoku s =
  let formula1 = List.flatten (List.init 9 (fun i ->
                                   List.init 9 (fun j ->
                                       [false, var i j 9])))
  in 
  let formula2 = List.flatten
                   (List.flatten (List.flatten (List.init 9 (fun i ->
                                                    List.init 9 (fun j ->
                                                        List.init 9 (fun n ->
                                                            let l = List.init (8 - j) (fun i' -> i' + j + 1) in
                                                            List.map (fun k -> [false, var i j n; false, var i k n]) l))))))
  in
  let formula3 = List.flatten
                   (List.flatten (List.flatten (List.init 9 (fun i ->
                                                    List.init 9 (fun j ->
                                                        List.init 9 (fun n ->
                                                            let l = List.init (8 - j) (fun i' -> i' + j + 1) in
                                                            List.map (fun k -> [false, var j i n; false, var k i n]) l))))))
  in
  let formula4 =
   
    
  formula3


(*

0,0 0,1 0,2 1,0  1,1 1,2 2,0  2,1 2,2 

 *)    

(*
i , j, k 
0, 0, 8 
1, 0, 
2, 0, 
3, 0,


 *)
