(* Check if a Church numeral is equal to zero *)

(* iszero : 'a church -> bool *)
let iszero c = match c with
  | Church f -> (f (fun _ -> false) true) 

(* Convert an integer to a Church numeral *)

(* create : int -> 'a church *)
let create c = match c with 
  | c -> Church(fun f x -> 
      let rec create' f x c = match c with 
        | 0 -> x 
        | c -> create' f (f x) (c - 1)
      in
      create' f x c)
                           


(* Convert a Church numeral to an integer *)

(* churchToInt : 'a church -> int *)
let churchToInt c = match c with
  | Church f -> (f ((+)1) 0)

(* Add two Church numerals *)

(* add : 'a church -> 'a church -> 'a church *)
let add c1 c2 = match c1, c2 with
  | Church f1, Church f2 -> Church(fun f x -> f1 f (f2 f x))

(* Multiply two Church numerals *)

(* mult : 'a church -> 'a church -> 'a church *)
let mult c1 c2 = match c1, c2 with
  | Church f1, Church f2 -> Church(fun f x -> f1 (f2 f) x)