(* ---------------------------------------------------- *)

(* Processing finite objects lazily is also useful;
   it corresponds to demand driving compution.
*)
(* ---------------------------------------------------- *)
(* We define next a lazy list; this list is possibly
   finite; this is accomplished by a mutual recursive
   datatype.

   'a lazy_list defines a lazy list; we can observe the 
   head and its tail. For the tail we have two options:
   we have reached the end of the list indicated by the 
   constructor None or we have not reached the end 
   indicated by the constructor Some and we expose
   another lazy list of which we can observe the head and the tail.  

*)

(* ---------------------------------------------------- *)         
(* Q1 *)

(* 
   val take : int -> 'a lazy_list -> 'a list 
*)
let rec take n s =
  if (n == 0) then []
  else s.hd :: (take' (n-1) (force s.tl)) 
and take' n s = match s with
  |None -> []
  |Some tl -> take n tl


(* val map : ('a -> 'b) -> 'a lazy_list -> 'b lazy_list
*)
let rec map f s =
  {hd = f s.hd;
   tl = Susp(fun () -> map' f (force s.tl))
  }
and map' f l = match l with 
  |None -> None
  |Some l' -> Some (map f l')



(* 
val append : 'a lazy_list -> ('a lazy_list) option susp -> 'a lazy_list
*)
let rec append s1 s2 = 
  {hd = s1.hd;
   tl = Susp(fun () ->  append' (force s1.tl) (force s2) )
  }
and append' s1 s2 = match s1,s2 with
  |None,s2' -> s2'
  |s1',None -> s1'
  |Some s1',Some s2' -> Some (append s1' (Susp(fun () -> s2)))


(* ---------------------------------------------------- *)
(* val interleave : 'a -> 'a list -> 'a list lazy_list *)
let rec interleave x l = match l with
  |[] ->{hd = [x]; tl =Susp(fun () -> None)}
  |h::t -> 
      {hd = x::h::t;
       tl = Susp(fun() -> Some (map (fun e->h::e) (interleave x t) ))
      }



(* ---------------------------------------------------- *)
(* val flatten : 'a lazy_list lazy_list -> 'a lazy_list = <fun>
*)
let rec flatten s = match force s.tl with
  |None -> s.hd
  |Some k -> append s.hd (Susp (fun () -> Some (flatten k)))


(* ---------------------------------------------------- *)
(* Permute *)
let rec permute l = match l with
  |[] ->{hd = l; tl =Susp(fun () ->None)}
  |h::t -> flatten (map (interleave h) (permute t))




(* ---------------------------------------------------- *)         
(* Q2 *)
                   
let rec hailstones n = 
  if(n mod 2 ==0) then {hd =n;
                        tl = Susp(fun()-> Some(hailstones (n/2)))}
  else {hd =n; tl =Susp(fun () -> Some (hailstones (3*n +1)))}