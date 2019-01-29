(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 1 *)
(* Helper function: given two expressions, we add or multiply
   them     *)
(* ————————————————————–————————————————————————————————————————– *)

let add e1 e2 = 
  let result = ref 0 in
  match e1, e2 with
  | Num(n1), Num(n2) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  | Num(n1), Plus(n2, _, _) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  | Num(n1), Times(n2, _, _) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  | Plus(n1, _, _), Num(n2) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  | Plus(n1, _, _), Plus(n2, _, _) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  | Plus(n1, _, _), Times(n2, _, _) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  | Times(n1, _, _), Num(n2) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  | Times(n1, _, _), Plus(n2, _, _) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  | Times(n1, _, _), Times(n2, _, _) -> (result:= !n1 + !n2; Plus(result, e1, e2))
  

let mult e1 e2 = 
  let result = ref 0 in
  match e1, e2 with
  | Num(n1), Num(n2) -> (result:= !n1 * !n2; Times(result, e1, e2))
  | Num(n1), Plus(n2, _, _) -> (result:= !n1 * !n2; Times(result, e1, e2))
  | Num(n1), Times(n2, _, _) -> (result:= !n1 * !n2; Times(result, e1, e2))
  | Plus(n1, _, _), Num(n2) -> (result:= !n1 * !n2; Times(result, e1, e2))
  | Plus(n1, _, _), Plus(n2, _, _) -> (result:= !n1 * !n2; Times(result, e1, e2))
  | Plus(n1, _, _), Times(n2, _, _) -> (result:= !n1 * !n2; Times(result, e1, e2))
  | Times(n1, _, _), Num(n2) -> (result:= !n1 * !n2; Times(result, e1, e2))
  | Times(n1, _, _), Plus(n2, _, _) -> (result:= !n1 * !n2; Times(result, e1, e2))
  | Times(n1, _, _), Times(n2, _, _) -> (result:= !n1 * !n2; Times(result, e1, e2))
(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 2                                                        *)
(* compute_column m f = c

   Given a spreadsheet m and a function f, compute the i-th value in
   the result column c by using the i-th value from each column in m.

   Example:
   m = [ [a1 ; a2 ; a3 ; a4] ;
         [b1 ; b2 ; b3 ; b4] ;
         [c1 ; c2 ; c3 ; c4] ]

  To compute the 2nd value in the new column, we call f with
  [a2 ; b2 ; c2]

   Generic type of compute_column:
     'a list list -> ('a list -> 'b) -> 'b list

   If it helps, you can think of the specific case where we have a
   spreadsheet containing expressions, i.e.
   compute_column: exp list list -> (exp list -> exp) -> exp list

   Use List.map to your advantage!

   Carefully design the condition when you stop.
*)
(* ————————————————————–—————————————————————————————————————————————– *)
let rec compute_column sheet (_, f) = 
  let rec trans list = match list with
    | [] -> []
    | []::xss-> trans xss
    | (x::xs)::xss -> (x::List.map List.hd xss) :: trans (xs :: List.map List.tl xss) 
  in
  List.map (fun col -> f col ) (initialize (trans (
      List.map 
        (fun row -> List.map (fun element -> ref (get_value element)) row) sheet)))

(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 3 *)
(* Implement a function update which given an expression will re-
   compute the values stored at each node. This function will be used
   after we have updated a given number.

   update  : exp -> unit

*)
(* ————————————————————–—————————————————————————————————————————————– *)

let update expr =
  let rec up expr = match expr with
    |Num(_) -> ()
    |Plus(r,r1,r2) -> (up r1; up r2;  r:= get_value r1 + get_value r2)
    |Times(r,r1,r2) -> (up r1; up r2;  r:= get_value r1 * get_value r2)
  in up expr

let update_sheet sheet = 
  List.fold_left (fun _ column ->  
      List.fold_left (fun _ e -> update e) () column) () sheet

(* EXTRA FUN:
   Our implementation traverses the whole expression and even worse
   the whole spreadsheet, if one number cell is being updated.

   If you are looking for a nice programming problem, think
   about how to update only those values which are parent nodes to the
   Number being updated. You might need to choose a different
   representation for expressions.

*)
(* ————————————————————–—————————————————————————————————————————————– *)
