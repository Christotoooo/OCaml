(* Question 1: let's compose something! *)

(* 1.1 Composition *)
(*
    fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
List.fold_right f [a1; ...; an] b is f a1 (f a2 (... (f an b) ...)). 
   
   *)
let compose (fs : ('a -> 'a) list): 'a-> 'a = fun x ->
  List.fold_right (fun f a -> f a) fs x
  
(* 1.2 Replication *) 
let rec replicate (n : int) : 'a -> 'a list = 
  fun x -> match n with
    |0 -> [] 
    |n' -> [x] @ (replicate (n'-1) x)

(* 1.3 Repeating *)

let repeat (n : int) (f : 'a -> 'a) : 'a -> 'a =
  fun x -> compose(replicate n f) x 
  

(* Question 2: unfolding is like folding in reverse *)

(* 2.1 Compute the even natural numbers up to an exclusive limit. *)
let evens (max : int) : int list =
  unfold (fun b -> (b, b+2)) (fun b -> max <= b) 0
    

(* 2.2 Compute the fibonacci sequence up to an exclusive limit. *)
let fib (max : int) : int list = match max with
  | 0 -> []
  | 1 -> []
  | max -> 1::(unfold (fun (a,b) -> (a+b, (b, a+b)))  (fun (a,b) -> max <= a+b) (0,1) )
  


(*val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
List.map2 f [a1; ...; an] [b1; ...; bn] is [f a1 b1; ...; f an bn]. 
Raise Invalid_argument if the two lists are determined to have different lengths.*)
(* 2.3 Compute Pascal's triangle up to a maximum row length. *)
let pascal (max : int) : int list list = 
  unfold (fun l-> l, (List.map2 (fun x y ->x+y) (0::l) (l@[0])))
    (fun l -> List.length l >max) [1]
  (* let getRow b l =
      unfold (fun (n,l) -> if (n=b || n=1) then(1,(n+1, l))
               else ((List.nth l (n-1) + List.nth l (n-2)), (n+1,l)))
        (fun (n,_) -> b<n) (1,l)
    in
    unfold (fun (b,l) -> (getRow b l, (b+1, getRow b l))) (fun (b,_)-> max<b) (1,[])
*)
(* 2.4 Implement the zip, which joins two lists into a list of tuples.
 * e.g. zip [1;2] ['a', 'c'] = [(1, 'a'); (2, 'c')]
 * Note that if one list is shorter than the other, then the resulting
 * list should have the length of the smaller list. *)
    (*let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list = match l1,l2 with
        |[],_ ->[]
        |_,[] ->[]
        |h1::t1,h2::t2 ->
            unfold (fun (a,b) -> ([(a,b)], (b, a+b)))  (fun (a,b) -> h1=[] || h2=[]) (h1,h2)
*)
        
let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  unfold (
    fun (l1, l2) -> 
      ((List.hd l1, List.hd l2), (List.tl l1, List.tl l2))
  ) 
    (fun (l1, l2) -> (l1 = []) || (l2 = []))
    (l1, l2)
