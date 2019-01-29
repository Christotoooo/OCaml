(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)


let new_account p =
  let current_password = ref p in
  let current_balance = ref 0 in
  let attempts = ref 0 in
  {
    update_passwd =(fun oldpwd newpwd->if(!current_password=oldpwd) 
                     then(current_password:=newpwd;attempts:=0)
                     else(attempts:= 1+ !attempts; raise wrong_pass));
    retrieve=(fun pwd amt->if(3 = !attempts) then (raise too_many_attempts)
               else if(!current_password <>pwd) 
               then (attempts:=!attempts +1; raise wrong_pass) 
               else(
                 if( !current_balance >amt) then(current_balance:=!current_balance -amt;attempts:=0)
                 else raise no_money
               ));
    deposit=(fun pwd amt2->if(3 = !attempts) then (raise too_many_attempts)
              else if(!current_password <>pwd) 
              then (attempts:=!attempts +1; raise wrong_pass) 
              else(
                (current_balance:=!current_balance +amt2;attempts:=0)
              ));
    print_balance=(fun pwd ->if(3 = !attempts) then (raise too_many_attempts)
                    else if (!current_password=pwd) then(attempts:=0;!current_balance)
                    else (attempts:=1 + !attempts; raise wrong_pass)
                  )
  }
;;



(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec catalan_I n =
  let counter = ref 0 in
  let rec catalan n = match n with
    | 0 -> counter:= 1+ !counter; 1
    | 1 -> counter:= 1+ !counter; 1
    | n -> counter:= 1+ !counter; sum (fun i -> catalan i * catalan (n - 1 - i)) (n - 1) 
  in
  {
    num_rec= !counter 
  ;
    result = catalan n
            
  
  }
;;


(* Q 2.2 : Memoization with a global store *)

let rec catalan_memo n =
  let rec catalan n = match n with
    |0-> if(Hashtbl.find_opt store 0 <> None) then 1
        else (Hashtbl.add store 0 1; 1)
    |1-> if(Hashtbl.find_opt store 1 <> None) then 1
        else (Hashtbl.add store 1 1; 1)
    |n-> if(Hashtbl.find_opt store n <> None) 
        then ((fun (Some num) -> num)(Hashtbl.find_opt store n))
        else (Hashtbl.add store n 
                (sum (fun i -> catalan i * catalan (n - 1 - i)) (n - 1)); 
              catalan n)
  in
  catalan n
;;


(* Q 2.3 : General memoization function *)
let memo f stats =
  let htbl = ref (Hashtbl.create 1000) in
  let rec g = (fun a -> match Hashtbl.find_opt !htbl a with
      | None -> (let result = f g a in
                 Hashtbl.add !htbl a result; 
                 stats.entries := !(stats.entries) +1; result;)
      | Some k -> (stats.lkp := !(stats.lkp) + 1; k))
  in g
;;


(* Q 2.4 : Using memo to efficiently compute the Hofstadter Sequence Q *)
let sta = ref {entries = ref 0; lkp = ref 0} 
let f g = function
  | 1 | 2 -> 1
  | n -> g (n - g (n - 1)) + g (n - g (n - 2)) 
let hof = ref (memo f !sta) 
    
    
let hofstadter_Q n = 
  (fun x -> ((!hof n), !sta)) memo 
    
;;
