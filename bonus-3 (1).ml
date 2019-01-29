(* Question 1: unify *)
let rec unify (t1 : tp) (t2 : tp) : unit =
  match t1, t2 with
  (* unifying identical concrete types does nothing *)
  | Int, Int
  | Bool, Bool -> ()
  (* For type constructors, recursively unify the parts *)
  | Arrow (t1, t1'), Arrow (t2, t2') ->
      (unify t1 t2 ; unify t1' t2')
  | Product tl1, Product tl2 ->
      if List.length(tl1) <> List.length(tl2) then raise (UnifError (UnifProductMismatch))
      else  List.iter2 (fun x y -> unify x y) tl1 tl2
  | TVar a, _ -> unifyVar a t2
  | _, TVar b -> unifyVar b t1
  (* All other cases are mismatched types. *)
  | _, _ -> unif_error @@ UnifMismatch (t1, t2)

(* Unify a variable with a type *)
and unifyVar a t = match !a with
  | Some a' -> (let t' = a' in unify t' t) 
  | None -> 
      (match t with
       | TVar b ->( match !b with
           |Some b' -> unifyVar a b' 
           |None -> if (a == b) then ()
               else a:= Some (TVar b)
         )
       | _ -> if (occurs a t) then raise (UnifError (UnifOccursCheckFails))
           else a:= Some t
      )

(* Question 2: infer
   Copy your code for infer from hw10, and then adjust it to work with
   the new definition of exp in the prelude.
*)
let rec infer (ctx : context) (e : exp) : tp = match e with
  | Var x -> (match lookup x ctx with
      | Some tp -> tp
      | None -> type_error (FreeVariable x))
  | I _ -> Int
  | B _ -> Bool
  | Primop (po, exps) -> 
      let (domain, range) =  primopType po in
      let rec check exps ts = match exps , ts with
        | [] , [] -> range
        | e::es , t::ts -> (unify (infer ctx e) t); check es ts
        | _ -> type_error InvalidPrimop
      in
      check exps domain

  | If (e, e1, e2) -> unify (infer ctx e) Bool; 
      unify (infer ctx e1) (infer ctx e2); 
      infer ctx e2

  | Fn (x,e) -> let a = ref None in 
      let ctx' = extend ctx (x, TVar a) in 
      Arrow (infer ctx' (Var x), infer ctx' e); 

  | Apply (e1, e2) -> let tp = ref None in
      unify (infer ctx e1) (Arrow (infer ctx e2, TVar tp)); TVar tp

                                                                                   
  | Rec (f, e) -> let a = ref None in
      let t = (infer (extend ctx (f, TVar a)) e) in 
      unify (TVar a) t; t
                                                                                                                                         
  | Tuple es -> Product (List.map (infer ctx) es)
  | Let ([], e) -> infer ctx e
  | Let (dec::decs, e) ->
      let ctx' = infer_dec ctx dec  in
      infer ctx' (Let(decs, e))

(** Extends the context with declarations made in Val or Valtuple. *)
and infer_dec ctx dec = match dec with
  | Val (e, x) -> 
      let t = ref None in 
      unify (TVar t) (infer ctx e); 
      extend ctx (x, TVar t)
        
  | Valtuple (e, nl) -> 
      (let t1 = infer ctx e in 
       let rec build t1 nl = match t1 with
         | Product [] -> 
             if nl =[] then []
             else unif_error (UnifProductMismatch) 
         | Product (h::t) ->
             (match nl with
              | []-> unif_error (UnifProductMismatch)
              | n::ns -> (n,h)::(build (Product t) ns) )
         |t-> unif_error (UnifMismatch (Product [], t))
       in
       (build t1 nl)@ctx
      )


