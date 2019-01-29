(* -------------------------------------------------------------*)
(* QUESTION 1 : Let's have cake!                                *)
(* -------------------------------------------------------------*)
(*Collaborated with Chicheng Zheng*)
(* allergy_free : ingredient list -> cupcake list -> cupcake list *)
let allergy_free allergens cupcakes = match allergens, cupcakes with
  |[],_ -> cupcakes
  |_,[] -> []
  |allergens,cupcakes -> List.filter ((function cupcake -> match cupcake with
      |Cupcake(price,weight,calories,[])-> true
      |Cupcake(price,weight,calories,ingredient) ->
          List.for_all (function ing -> not (List.exists (function all -> all = ing) allergens)) ingredient
    )) cupcakes

(* -------------------------------------------------------------*)
(* QUESTION 2 : Generic Tree Traversals                         *)
(* -------------------------------------------------------------*)
(*helper method*)
let uncurry f = function (x,y,z) -> f x y z

(* map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f t = match t with
  |Empty -> Empty
  |Node(a,Empty,Empty) -> Node((f a),Empty,Empty)
  |Node(a,atree1,atree2)-> Node((f a),(map_tree f atree1),(map_tree f atree2))

(* delete_data : ('a * 'b) tree -> 'a tree *)
let delete_data t = match t with
  |Empty -> Empty
  |t -> map_tree (function (a,b) -> a) t

(* fold_tree : ('a * 'b ' * 'b -> 'b) -> 'b -> 'a tree -> 'b *)
let rec fold_tree f e t = 
  match t with 
  |Empty -> e
  |Node(a,atree1,atree2) -> f a (fold_tree f e atree1) (fold_tree f e atree2) 

(* size : 'a tree -> int *)
let size tr=match tr with
  |Empty -> 0
  |Node(n,l,r)-> fold_tree (fun n x y -> 1+ x + y) 0 tr


(* reflect : 'a tree -> 'a tree *)
let reflect tr = match tr with
  |Empty->Empty
  |Node(n,l,r)-> fold_tree (fun n x y -> Node(n,y,x)) Empty tr

(* inorder : 'a tree -> 'a list *)
let inorder tr = match tr with
  |Empty->[]
  |Node(n,l,r) -> fold_tree(fun n x y-> x@[n]@y) [] tr
