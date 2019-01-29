(***** Question 1a: unused variables *****)
let together (dec,xx) = match xx with
  |Let(list,exp) -> Let(dec::list,exp)
  |_->raise NotImplemented




let rec unused_vars e = match e with
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) ->
      union (unused_vars e, union (unused_vars e1, unused_vars e2))
  | Primop (po, args) ->
      List.fold_right (fun e1 e2 -> union (unused_vars  e1, e2)) args []
  | Apply (e1, e2) -> union (unused_vars e1, unused_vars e2)
  | Fn (x, _, e) | Rec (x, _, e) ->
      if member x (freeVariables e) then
        unused_vars e
      else
        union ([x], unused_vars e)

  | Tuple exps ->( match exps with 
      |[] -> []
      |x::[] -> unused_vars x
      |(x::xs) as l -> List.fold_right (fun e1 e2 -> union (unused_vars e1, e2 )) l []
    )
      
  
  | Let ([], e) -> unused_vars e

  | Let (Val (e,x)::decs, e2) ->let temp = Let(decs,e2) in
      let partial_result = union(unused_vars e, unused_vars temp) in
      if member x (freeVariables temp) then partial_result
      else union([x],partial_result)
      
                                 
  | Let (Valtuple (e,xl)::decs, e2) -> let temp = Let (decs,e2) in
      let partial_result = union(unused_vars e, unused_vars temp) in
      let names = List.filter (fun x-> not (member x (freeVariables temp))) xl in
      union(names, partial_result)

(* Question 1b: write your own tests for unused_vars *)

let unused_vars_tests : (exp, name list) tests =
  [ ( Fn ("x", Int, Tuple [ Var "x"; Var "x" ] )
    , []
    );( (Var ("x")), []);( (I (10)), []);( (B (true)), []);
    ( Let([],Var("x")),[])
  ]

(* Question 2a: substitution *)

(** Applies the substitution s to each element of the list a. *)
let rec substArg s a = List.map (subst s) a

(** Applies the substitution (e', x), aka s, to exp.
    To implement some of the missing cases, you may need to use the
    `rename` function provided below. To see why, see the comment
    associated with `rename`.
*)
and subst ((e',x) as s) exp =  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop(po, args) -> Primop(po, substArg s args)
  | If(e, e1, e2) ->
      If(subst s e, subst s e1, subst s e2)
  | Apply (e1, e2) -> Apply (subst s e1, subst s e2)
  | Fn (y, t, e) ->
      if y = x then
        Fn (y, t, e)
      else
      if member y (freeVariables e') then  (**[let y = 3 in y+4/x](fun y' -> y'+x)      *)
        let (y,e1) = rename(y,e) in
        Fn (y, t, subst s e1)
      else
        Fn(y, t, subst s e)
  | Rec (y, t, e) ->
      if y = x then
        Rec (y, t, e)
      else
      if member y (freeVariables e') then
        let (y, e1) = rename (y,e) in
        Rec (y, t, subst s e1)
      else
        Rec (y, t, subst s e)

  | Tuple es -> Tuple(List.map (fun x -> subst s x) es)

  | Let([], e2) -> Let ([], subst s e2)

  | Let(dec1::decs, e2) ->
      (match dec1 with
       | Val(exp, name) -> let exp' = subst s exp in
           if name = x then Let((Val(exp',name)::decs),e2) 
           else
           if member name (freeVariables e') then
             let (name,rest) = rename (name,(Let(decs,e2))) in
             together(Val(exp',name), subst s rest)
           else together(Val(exp', name), subst s (Let(decs,e2)))

       | Valtuple(exp, names) -> let exp' = subst s exp in
           if member x names then Let((Valtuple(exp',names)::decs), e2)
           else let (names,rest) = renameList names e' (Let(decs, e2)) in
             together(Valtuple(exp', names), subst s rest)
      )

and substList l e = match l with
  | [] -> e
  | (x,e')::pairs ->
      subst (x,e') (substList pairs e)
        

(** Replaces the variable x with a fresh version of itself.

    This is an important operation for implementing substitutions that
    avoid capture.
    e.g. If we naively compute [y/x](fun y => x) we get (fun y => y),
    but this doesn't have the right interpretation anymore; the
    variable y that we plugged in got "captured" by the `fun y`
    binder.
    The solution is to observe that the variable y introduced by the
    fun-expression *appears free* in the expression we are substituting for x.
    In this case, we must rename the bound variable y (introduced by
    `fun y`).
    e.g We want to compute [y/x](fun y => x). We first rename the
    bound variable y, and instead compute [y/x](fun y1 => x).
    This gives (fun y1 => y), which has the right interpretation.
*)
and rename (x, e) =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

(** Performs multiple renamings on the same expression. *)
and renameAll e = match e with
  | ([], e) -> ([], e)
  | (x::xs, e) ->
      let (x', e) = rename (x, e) in
      let (xs', e) = renameAll (xs, e) in
      (x' :: xs', e)

and renameList names e' exp =
  if List.exists (fun name -> member name (freeVariables e')) names then
    renameAll(names, exp)
  else
    (names, exp)

(* Question 2b: write your own tests for subst *)
let subst_tests : (subst * exp, exp) tests =
  [ ( ( (Var "x", "y"), Tuple [ Var "y" ] )
    , Tuple [ Var "x" ]
    );

    ( ( (Primop(Plus, [Var "y"; Var "x"]), "x"), Fn ("y", Int, Var "x" ))
    , Fn("1y", Int, Primop(Plus, [Var "y"; Var "x"])  ));
    (* fun y -> x *)

    ( ( (Var "x", "y"), Let([Val (I 3, "y")], Primop(Plus, [Var "y"; I 1])))
    , Let([Val (I 3, "y")], Primop(Plus, [Var "y"; I 1])));

    (* let y = 3 in y + 1 *)

    ( ( (Var "x", "y"), Let([Val (I 1, "x")], Primop(Plus, [Var "y"; Var "x"])))
    , Let([Val (I 1, "1x")], Primop(Plus, [Var "x"; Var "1x"])));

    (* subst (Var "x", "y") (Let([Val (I 1, "x")], Primop(Plus, [Var "y"; Var "x"]))) *)
    (* let x = 1 in x + y *)
    (* let x1 = 1 in x1 + x *)

    ( ( (Var "x", "y"), Let([Val (I 1, "x"); Val (I 4, "y")], Primop(Plus, [Var "y"; Var "x"])))
    , Let ([Val (I 1, "x"); Val (I 4, "y")], Primop(Plus, [Var "y"; Var "x"])));

   (* subst (Var "x", "y") (Let([Val (I 1, "x"); Val (I 4, "y")], Primop(Plus, [Var "y"; Var "x"]))) *)
   (* let x = 1, y = 4 in x + y *)
   (* let x = 1, y = 4 in x + y *)

    ( ( (Var "x", "z"), Let([Valtuple(Tuple([I 3; I 4]),["y"; "z"])], Primop(Plus, [Var "x"; Var "y"])))
    , Let ([Valtuple (Tuple ([I 3; I 4]), ["y"; "z"])], Primop(Plus, [Var "x"; Var "y"])));

   (* subst (Var "x", "z") (Let([Valtuple(Tuple([I 3; I 4]),["y"; "z"])], Primop(Plus, [Var "x"; Var "y"]))) *)
   (* let (y, z) = (3, 4) in x + y *)
   (* let (y, z) = (3, 4) in x + y *)

    ( ( (Var "x", "y"), Let ([Val (Fn ("y", Int, I 2), "x")], Primop(Plus, [Var "x"; Var "z"])))
    , Let ([Val (Fn ("x", Int, I 2), "1x")], Primop(Plus, [Var "1x"; Var "z"])));
    (* let x = (fun y -> 2) in x + z *)
    (* let x1 = (fun x -> 2) in x1 + z *)


    ( ( (Var "x", "z"), Let([Valtuple(Tuple([I 3; I 4]),["y"; "z"]); Val( I 5, "x")], Primop(Plus, [Var "x"; Var "y"])))
    , Let ([Valtuple (Tuple ([I 3; I 4]), ["y"; "z"]); Val( I 5, "x")], Primop(Plus, [Var "x"; Var "y"])));

    (* subst (Var "x", "z") (Let([Valtuple(Tuple([I 3; I 4]),["y"; "z"])], Primop(Plus, [Var "x"; Var "y"]))) *)
    (* let (y, z) = (3, 4), x = 5 in x + y *)
    (* let (y, z) = (3, 4), x = 5 in x + y *)

    ( ( (Var "x", "z"), Let([Valtuple(Tuple([I 3; I 4]),["y"; "a"]); Val( I 5, "x")], Primop(Plus, [(Primop(Plus, [Var "x"; Var "y"])); Var "z"])))
    , Let ([Valtuple (Tuple ([I 3; I 4]), ["y"; "a"]); Val( I 5, "1x")], Primop(Plus, [(Primop(Plus, [Var "1x"; Var "y"])); Var "x"])));

    (* subst (Var "x", "z") (Let([Valtuple(Tuple([I 3; I 4]),["y"; "a"]); Val( I 5, "x")], Primop(Plus, [Var "x"; Var "y"; Var "z"]))) *)
    (* let (y, a) = (3, 4), x = 5 in x + y + z *)
    (* let (y, a) = (3, 4), x1 = 5 in x1 + y + x*)

    (* let (y, a) = 3, x = 5 in y + x *)

    ( ( (I 3, "x"), Let([Val (I 3, "y")], Primop(Plus, [Var "y"; I 1])))
    , Let([Val (I 3, "y")], Primop(Plus, [Var "y"; I 1]))); 


  ]

(* Question 3a: evaluation *)

let rec evalList (exps : exp list) =
  List.map eval exps

and eval (exp : exp) : exp = match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
  appear during evaluation since they should be substituted away when
  eliminating binding constructs, e.g. function applications and lets.
  Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck ("Free variable (" ^ x ^ ") during evaluation"))

  (* primitive operations +, -, *, <, = *)
  | Primop(po, args) ->
      let argvalues = evalList args in
      (match evalOp(po, argvalues) with
       | None -> raise (Stuck "Bad arguments to primitive operation")
       | Some v -> v)

  | If(e, e1, e2) ->
      (match eval e with
       | B true -> eval e1
       | B false -> eval e2
       | _ -> raise (Stuck "Scrutinee of If is not true or false"))

  | Rec (f, _, e) -> eval (subst (exp, f) e)  (*??????????????????*)
  | Apply (e1, e2) ->
      (match eval e1 with
       | Fn(x,_,e) -> eval (subst (e2,x) e)
       | _ -> raise (Stuck "Left term of application is not an Fn"))

  | Tuple es -> Tuple (evalList es) 
      (*(let rec eval_list es = match es with
           | [] -> []
           | h::t -> (eval h)::(eval_list t)
         in
         Tuple (eval_list es)
        )*)
      
      
  | Let([], e2) -> eval e2
                     
                     
  | Let(dec1::decs, e2) ->
      (match dec1 with
       | Val(e1, x) -> eval (subst (e1,x) (Let(decs,e2))) 
           
       | Valtuple(e1, xs) -> 
           (let e2' = Let(decs,e2) in 
            let rec subst_tuple e1' xs = 
              match e1' with
              | Tuple [] -> 
                  if xs = [] then []
                  else raise (Stuck "dimensions not match 1")
              | Tuple (h::t) -> 
                  ( match xs with
                    | [] -> raise (Stuck "dimensions not match 2")
                    | x::xs' -> (h,x)::(subst_tuple (Tuple t) xs')
                  )
              | _ -> raise (Stuck "not tuple")
            in
            let subst_list = subst_tuple (eval e1) xs in 
            let rec sub subst_list e2' = match subst_list with
              | [] -> eval e2'
              | h::t -> sub t (subst h e2')
            in
            sub subst_list e2' 
           ) 
      )
(* Question 3b: write your own tests for eval *)

let eval_tests : (exp, exp) tests =
  [ ( Tuple [ I 3; I 3 ], Tuple [ I 3; I 3 ] ); 
    (If (B true, I 5, I 3), I 5);
    (Primop (Plus, [I 3; I 2]), I 5); 
    (Let ([Val(Primop(Plus, [I 3; I 2]), "z")], Primop(Minus,[Var "z"; I 1])),
     I 4);
    (Let ([], I 3), I 3) 
  ]

(* Question 4a: type inference *)

let rec infer ctx e : tp = match e with
  | Var x -> (try lookup x ctx
              with NotFound -> raise (TypeError ("Found free variable")))
  | I _ -> Int
  | B _ -> Bool
  | Primop (po, exps) ->
      let (domain, range) =  primopType po in
      let rec check exps ts = match exps , ts with
        | [] , [] -> range
        | e::es , t::ts ->
            let t' = infer ctx e in
            if t' = t then check es ts
            else type_mismatch t t' 
      in
      check exps domain

  | If (e, e1, e2) ->
      (match infer ctx e with
       | Bool -> let t1 = infer ctx e1 in
           let t2 = infer ctx e2 in
           if t1 = t2 then t1
           else type_mismatch t1 t2
       | t -> type_mismatch Bool t)

  | Fn (x,t,e) -> Arrow(t, infer (extend ctx (x,t)) e)

  | Apply (e1, e2) ->( match infer ctx e1 with
      |Arrow(t,t') -> 
          let te2 = infer ctx e2 in
          if te2 = t then t'
          else type_mismatch t te2
      |twrong -> raise (TypeError ("Wrong Apply"))
    )
    
                        
  | Rec (f, t, e) -> 
      let extension = extend ctx (f,t) in
      let te = infer extension e in
      if t = te then t
      else raise (TypeError("Wrong Rec"))
      
    
  | Tuple es -> Product(List.map (fun x -> infer ctx x)  es)
      
  | Let ([], e) -> infer ctx e
  
  | Let (dec::decs, e) ->
      let ctx' = infer_dec ctx dec  in
      infer ctx' (Let(decs, e))

(** Extends the context with declarations made in Val or Valtuple. *)
and infer_dec ctx dec = match dec with
  | Val (e, x) -> 
      let te = infer ctx e in
      extend ctx (x, te)
      
  | Valtuple (e, nl) ->
      (let t1 = infer ctx e in
       let rec build t1 nl = match t1 with
         | Product [] -> if nl=[] then [] else raise (TypeError "dimensions not match")
         | Product (h::t) ->
             (match nl with
              | [] -> raise (TypeError "dimensions do not match")
              | n::ns -> (n,h)::(build (Product t) ns) )
         | _ -> raise (TypeError "not tuple")
       in
       (build t1 nl) @ ctx
      )


        (*
          | Valtuple (e, nl) -> (
              let te = infer ctx e in
              let rec infer_tuple te' nl = match te' nl with
                |Product h1::t1 as tl, h2::t2 as nl ->
                    if (List.length(tl) != List.length(nl))
                    then raise (TypeError("Incompatible lengths"))
                    else
                      let newlist = toTuple(nl,tl) in
                      extend_list ctx newlist
              in
              infer_tuple te nl
            )*)



        (*  match nl with
           |Tuple[] -> if nl =[] then ctx
               else raise (TypeError("Wrong Valtuple 1"))
           |Tuple (h::t) ->(match nl with
               |[]-> raise (TypeError("Wrong Valtuple 2"))
               |h'::t' ->
             )

        )*)



(* Question 4b: write your own tests for infer *)

let infer_tests : (context * exp, tp) tests =
  [ ( ([], Tuple [ I 3; I 3 ])
    , Product [ Int; Int ]
    ); (([],I(1)), Int );(([],B(true)), Bool); (([],I(2)), Int );(([],I(3)), Int )
  ]
