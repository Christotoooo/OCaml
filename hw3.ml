(* -------------------------------------------------------------*)
(* QUESTION 1 : String manipulation  [20 points]                *)
(* -------------------------------------------------------------*)

(* string_explode : string -> char list *)
let string_explode s =
  tabulate (function x -> s.[x]) (String.length s)

(* string_implode : char list -> string *)
let string_implode l =
  String.concat "" (List.map (Char.escaped) l) 
  
(* -------------------------------------------------------------*)
(* QUESTION 2 : Insert a string into a dictionary  [20 points]  *)
(* -------------------------------------------------------------*)

(*Worked with Chicheng Zheng*)
(* Insert a word into a dictionary. Duplicate inserts are allowed *)

let  insert s t =
  (* ins: char list * char trie list -> char trie list *)
  let rec ins l t = match (l,t) with
    |([],_) -> (Empty::t)
    |(l,[]) -> (unroll l)
    |(l,[Empty]) -> (Empty::(unroll l))
    |(l,Empty::c) -> (Empty::(ins l c))
    |(l0::l1, [Node(c,((c1::c2) as c0))]) -> 
        if (l0 = c) then [Node(l0,(ins l1 c0))] 
        else (unroll (l0::l1)) @ [Node(c,c0)] 
    |(l0::l1, (Node(c,((c1::c2) as c0)) as n0)::n1) -> 
        if (l0 = c) then [Node(l0,(ins l1 c0))] @ n1 
        else [n0] @(ins (l0::l1) n1) 
  in
  ins (string_explode s) t 
(* -------------------------------------------------------------*)
(* QUESTION 3 : Look up a string in a dictionary   [20 points]  *)
(* -------------------------------------------------------------*)

(* Look up a word in a dictionary *)

let lookup s t =
  (* lkp : char list -> char trie list -> bool *)
  let rec lkp l t = match (l,t) with
    |[],[] ->false
    | [], t0::t1 -> if (contains_empty t) then true else false 
    |(l,[Empty]) -> false 
    |h::tail, [] -> false
    |h::tail, Node (c, [])::n -> lkp (h::tail) n
    |h::tail, Empty::n1 -> lkp (h::tail) n1 
    |h::tail,[Node(c0,((c1::c2) as c))] ->
        if (h=c0) then (lkp tail c) else false 
    |h::tail,(Node(c0,((c1::c2) as c)))::n1 ->
        if (h=c0) then (lkp tail c) else lkp (h::tail) n1
  in
  lkp (string_explode s) t

(* -------------------------------------------------------------*)
(* QUESTION 4 : Find all strings in a dictionary   [OPTIONAL]   *)
(* -------------------------------------------------------------*)

(* Find all strings which share the prefix p *)

let find_all prefix t =
  (* find_all' : char list -> char trie list -> char list list *)
  let rec find_all' l t =
    raise NotImplemented
  in
  raise NotImplemented

(* -------------------------------------------------------------*)
(* QUESTION 5 :  Logic functions   [OPTIONAL]                   *)
(* -------------------------------------------------------------*)

(* eval: labeled_pred -> labeled_pred -> int -> int -> bool *)
let eval (_, (p : int -> bool)) (_, (q : int -> bool)) (n : int) =
  raise NotImplemented
