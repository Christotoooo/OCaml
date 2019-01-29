let rec parseExp toklist = match toklist with
  | [] -> raise (Error "Expected an expression: Nothing to parse")
  | _ ->
      try parseSExp toklist
      with
      | SumExpr (exp, [SEMICOLON]) -> exp
      | _ -> raise (Error "Expected a single semicolon")

and parseSExp toklist = match toklist with
    (*|[] -> raise (Error "Expected an expression: Nothing to parse")*)
  |_ -> 
      try parsePExp toklist
      with
      | ProdExpr(exp, PLUS::xs) ->
          (try parseSExp xs
           with 
           | SumExpr (exp1, xs) -> raise (SumExpr(Sum(exp, exp1), xs)))
      | ProdExpr (exp, SUB::xs) ->
          (try parseSExp xs 
           with 
           | SumExpr(exp1,xs) -> raise (SumExpr(Minus(exp, exp1), xs)))
      | ProdExpr(exp, xs) -> raise (SumExpr (exp, xs))
      | _ -> raise (Error "Expected a single plus or sub") 

and parsePExp toklist = match toklist with
  | [] -> raise (Error "Expected an expression: Nothing to parse")
  | _ -> 
      try parseAtom toklist
      with
      | AtomicExpr (exp, TIMES::xs) -> 
          (try parsePExp xs with 
             ProdExpr (exp1, xs) -> raise (ProdExpr(Prod(exp, exp1), xs))) 
      | AtomicExpr (exp, DIV::xs) -> 
          (try parsePExp xs with 
             ProdExpr (exp1, xs) -> raise (ProdExpr(Div(exp, exp1), xs))) 
          
      | AtomicExpr (exp, xs) -> raise (ProdExpr(exp, xs))     
      | _ -> raise (Error "Expected a single times or div")
 
           
and parseAtom toklist = match toklist with
  | [] -> raise (Error "Expected an expression: Nothing to parse")
  | INT (n)::xs -> raise (AtomicExpr(Int (n), xs))
  | LPAREN::xs ->
      (
        try parseSExp xs with
        | SumExpr (exp, RPAREN::xs) -> raise (AtomicExpr(exp, xs))
        | _ ->raise (Error "Expected a single right parentheses")
      )
  | _ -> raise (Error "Expected a single left parentheses")
         
;;
(*let rec parseExp toklist = match toklist with
    | [] -> raise (Error "Expected an expression: Nothing to parse")
    | _ ->
        try parseSExp toklist
        with
        | SumExpr (exp, [SEMICOLON]) -> exp
        | _ -> raise (Error "Expected a single semicolon")

 and parseSExp toklist = match toklist with
   | [] -> raise (Error "Expected an expression: Nothing to parse")
   | _ -> try parsePExp toklist with 
     | ProdExpr (exp, PLUS::xs) -> 
         (try parseSExp xs with 
            SumExpr (exp1, xs) -> raise (SumExpr(Sum(exp, exp1), xs))) 
     | ProdExpr (exp, SUB::xs) -> 
         (try parseSExp xs with 
            SumExpr (exp1, xs) -> raise (SumExpr(Minus(exp, exp1), xs)))
     | ProdExpr (exp, xs) -> raise (SumExpr (exp, xs))
     | _ -> raise (Error "Expected a single plus or sub")

 and parsePExp toklist = match toklist with
   | [] -> raise (Error "Expected an expression: Nothing to parse")
   | _ -> 
       try parseAtom toklist
       with
       | AtomicExpr (exp, TIMES::xs) -> 
           (try parsePExp xs with 
              ProdExpr (exp1, xs) -> raise (ProdExpr(Prod(exp, exp1), xs))) 
       | AtomicExpr (exp, DIV::xs) -> 
           (try parsePExp xs with 
              ProdExpr (exp1, xs) -> raise (ProdExpr(Div(exp, exp1), xs))) 
          
       | AtomicExpr (exp, xs) -> raise (ProdExpr(exp, xs))     
       | _ -> raise (Error "Expected a single times or div")

 and parseAtom toklist = match toklist with
   | [] -> raise (Error "Expected an expression: Nothing to parse") 
   | INT (n)::xs -> raise (AtomicExpr(Int (n), xs)) 
   | LPAREN::xs -> 
       (try parseSExp xs with
        | SumExpr (exp, RPAREN::xs) -> raise (AtomicExpr(exp, xs)) 
        | _ -> raise (Error "Expected a single right parentheses"))
   | _ -> raise (Error "Expected a single left parentheses")
 ;;*)

(* parse : string -> exp *)
let parse string =
  parseExp (lex string) ;;

(* eval : string -> int *)
let eval e = eval' (parse e) ;;
