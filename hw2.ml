(* --------------------------------------------------------------------*)
(* QUESTION 1: House of Cards                                          *)
(* --------------------------------------------------------------------*)

(* Q1: Comparing cards *)
(* Comparing two ranks *)
let dom_rank (r1 : rank) (r2 : rank) = match (r1,r2) with
  |(Ace,King) -> true
  |(Ace,Queen) -> true
  |(Ace,Jack) -> true
  |(Ace,Ten) -> true
  |(Ace,Nine) -> true
  |(Ace,Eight) -> true
  |(Ace,Seven) -> true
  |(Ace,Six) -> true
  |(King,Queen) -> true
  |(King,Jack) -> true
  |(King,Ten) -> true
  |(King,Nine) -> true
  |(King,Eight) -> true
  |(King,Seven) -> true
  |(King,Six) -> true
  |(Queen,Jack) -> true
  |(Queen,Ten) -> true
  |(Queen,Nine) -> true
  |(Queen,Eight) -> true
  |(Queen,Seven) -> true
  |(Queen,Six) -> true
  |(Jack,Ten) -> true
  |(Jack,Nine) -> true
  |(Jack,Eight) -> true
  |(Jack,Seven) -> true
  |(Jack,Six) -> true
  |(Ten,Nine) -> true
  |(Ten,Eight) -> true
  |(Ten,Seven) -> true
  |(Ten,Six) -> true
  |(Nine,Eight) -> true
  |(Nine,Seven) -> true
  |(Nine,Six) -> true
  |(Eight,Seven) -> true
  |(Eight,Six) -> true
  |(Seven,Six) -> true
  |(s1,s2) -> (s1=s2)
  

(* Comparing two cards (r1, s1) and (r2, s2) *)
let dom_card (c1 : card) (c2 : card) = match (c1,c2) with
  |((_,Spades),(_,Hearts)) ->true
  |((_,Spades),(_,Diamonds)) ->true
  |((_,Spades),(_,Clubs)) ->true
  |((_,Hearts),(_,Diamonds)) ->true
  |((_,Hearts),(_,Clubs)) ->true
  |((_,Diamonds),(_,Clubs)) ->true
  |((r1,Spades),(r2,Spades)) -> if dom_rank r1 r2 then true else false
  |((r1,Hearts),(r2,Hearts)) -> if dom_rank r1 r2 then true else false
  |((r1,Diamonds),(r2,Diamonds)) -> if dom_rank r1 r2 then true else false
  |((r1,Clubs),(r2,Clubs)) -> if dom_rank r1 r2 then true else false
  |((r1,s1),(r2,s2)) -> if s1<>s2 then false else false
    
  

(* Q2: Insertion Sort – Sorting cards in a hand *)
let rec insert (c : card) (h : hand) : hand = match (c,h) with 
  |(c0,Empty) -> Hand(c0,Empty)
  |(c1,Hand(c2,h2)) ->
      if (dom_card c1 c2) then Hand(c1,h)
      else Hand(c2,(insert c1 h2))
  

let rec sort (h : hand) : hand = match h with
  |Empty -> h
  |Hand(c1,h1) -> (insert c1 (sort h1))
  
  (*Q3 helper method extracting element at nth place of a list*) 
let rec nth n l = match l with
  |  [] -> raise Domain
  | x :: l -> if n <= 0 then x else nth (n - 1) l
          
let produce_card (s:suit) (r:rank): card= (r,s)
  
let rec one_suit (s:suit) (ranks:rank list): card list= match (s,ranks) with
  |(s,[])-> []
  |(s,h::t)-> [(produce_card s h)] @ (one_suit s t)
  
            
(* Q3: Generating a deck of cards *)
let rec generate_deck (suits : suit list) (ranks : rank list) : card list = 
  match (suits,ranks) with
  | ([],r)->[]
  | (h::t,r)-> (one_suit h r) @ (generate_deck t r)
  

  
(* Q4: Shuffling a deck of cards *)
let first_part (a,b) = a
let second_part (a,b) = b 
let rec split (deck : card list) (n : int) : card * card list =
  if(n>=0) then match deck,n with
    |[],_-> raise Domain
    |[card0],0-> (card0,[])
    |[card1],_-> raise NotImplemented
    |(card2::rem_list),0 -> (card2,rem_list)
    |(card3::rem_list),n -> (first_part(split rem_list (n-1)),card3::second_part(split rem_list (n-1)))
  else raise Domain
      
let rec select deck n = match (deck,n) with
  |([],_) -> []
  |([d1],n1) -> if (n<>0) then raise Domain else [d1]
  |(d2,n2) -> (first_part (split d2 n2)::(select (second_part (split d2 n2))
                                            (Random.int ((List.length d2)-1))))
  
let shuffle (deck : card list) : card list =
  let size = List.length deck in
  match deck with
  |[] -> [] 
  |deck -> select deck (Random.int size)

(* --------------------------------------------------------------------*)
(* QUESTION 2: Sparse Representation of Binary Numbers                 *)
(* ------------------------------------------------------------------- *)
(*Cooperated with Chicheng Zheng and Qingchuan Ma*)
let rec check_suivant list = match list with 
  | h::[] -> [2*h]
  | h1::(h2::t) -> if (h2 = 2*h1) then check_suivant (h2::t) else [2*h1]@(h2::t) 
  
(* Q1: Incrementing a sparse binary number *)
let inc (ws : nat ) : nat = 
  match ws with
  |[] -> [1]
  |[0] -> [1]
  |1::l -> check_suivant ws
  |ws -> 1::ws
          
         

let rec dec_helper (ws: nat) (number: int): nat = match (ws,number) with
  |(list,1) -> list 
  |(list,n) -> dec_helper ((n/2)::list) (n/2)
      
(* Q2: Decrementing a sparse binary number *)
let dec (ws : nat) : nat = match ws with
  |[] -> raise Domain
  |1::t0 -> t0
  |h1::t1 -> dec_helper t1 h1
  

(* Q3: Adding sparse binary numbers *)
let rec add (m : nat) (n : nat) : nat  =
  let n = dec n in 
  let m = inc m in 
  if (n=[]) then m 
  else add m n

(* Q4: Converting to integer - tail recursively *)
let rec toInt (n : nat) (acc : int) : int =
  let n = dec n in
  let acc = acc +1 in
  if(n=[]) then acc
  else toInt n acc

let sbinToInt (n : nat) : int =
  toInt n 0
