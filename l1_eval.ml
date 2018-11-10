type variable = string;;

(* Outros operadores binário e unários podem ser adicionados a linguagem *)
type bop = 
        Sum   (* + *)
    |   Sub   (* - *)
    |   Mult  (* x *)
    |   Div   (* / *)
    |   And   (* and *)
    |   Or    (* or *)
    |   Eq    (* = *)
    |   Df    (* <> *)
    |   Lt    (*  < *)
    |   Le    (* <= *)
    |   Gt    (* > *)
    |   Ge    (* >= *);;
     
type uop = Not;;

type expr = 
        Ncte of int 
    |   Bcte of bool
    |   Binop of bop * expr * expr
    |   Unop of uop * expr
    |   Pair of expr * expr  
    |   First of expr
    |   Second of expr
    |   If of expr * expr * expr 
    |   Var of variable 
    |   App of expr * expr      (* "apply" e1(e2) *)
    |   Lam of variable * expr (* Function "lambda" *)
    |   Let of variable * expr * expr
    |   Lrec of variable *  variable * expr * expr
    |   Nil
    |   Cons of expr * expr  (* List Contructor *)
    |   IsEmpty of expr
    |   Hd of expr
    |   Tl of expr
    |   Raise
    |   Try of expr * expr;;
        
type result =
        Vnum of int 
    |   Vbool of bool 
    |   Vpair of result * result
    |   Vnil
    |   Vcons of result * result                    (* List contructor value *)
    |   Vclos of variable * expr * env              (* Closure *)
    |   Vrclos of variable * variable * expr * env (* RECURSIVE closure*)
    |   RRaise
and
    env = (variable * result) list;;

(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies;;
exception ExecutionError;;
exception NoValueFound;;
exception NoValidValue;;
exception NoValidOperation;;
exception NoValidList;;

let rec findValueInEnv envi varName = 
    match envi with
    | envHead::envTail -> 
        (

            match envHead with 
            | (envVarName, envVarValue) -> 
                if envVarName = varName
                    then envVarValue 
                else 
                    findValueInEnv envTail varName
        )
    | [] -> raise NoValueFound;;

(* BIG STEP L1 *)
let rec eval envi e = 
    match e with
    |   Ncte(n) -> Vnum(n)
    |   Bcte(b) -> Vbool(b)
    |   Nil -> Vnil
    |   Var(name) -> findValueInEnv envi name
    |   If(boolExpression, e1, e2) -> 
        (
            let boolValue = eval envi boolExpression in
            match boolValue with
            |   Vbool(true) -> eval envi e1
            |   Vbool(false) -> eval envi e2
            |   _ -> raise NoValidValue
        )
    |   Pair(e1, e2) -> 
        (
            let v1 = eval envi e1 in
            let v2 = eval envi e2 in
                Vpair(v1, v2)
        )
    |   First(e) ->
        (
            let v = eval envi e in
            match v with
            |   Vpair(f, _) -> f
            |   Vnil -> RRaise
            |   _ -> raise ExecutionError
        )
    |   Second(e) ->
        (
            let v = eval envi e in
            match v with
            |   Vpair(_, s) -> s
            |   Vnil -> RRaise
            |   _ -> raise ExecutionError
        )
    |   Unop(op, e) -> 
        (
            let v = eval envi e in
                match (op, v) with
                | (Not, Vbool(b)) -> Vbool(not b)
                | _ -> raise NoValidOperation
        )
    |   Binop(op, e1, e2) -> 
        (
            let v1 = eval envi e1 in
            let v2 = eval envi e2 in
                match (op, v1, v2) with 
                    |   (Sum, Vnum(n1), Vnum(n2)) -> Vnum(n1 + n2)
                    |   (Sub, Vnum(n1), Vnum(n2)) -> Vnum(n1 - n2)
                    |   (Mult, Vnum(n1), Vnum(n2)) -> Vnum(n1 * n2)
                    |   (Div, Vnum(n1), Vnum(n2)) -> Vnum(n1 / n2)
                    |   (And, Vbool(b1), Vbool(b2)) -> Vbool(b1 && b2)
                    |   (Or, Vbool(b1), Vbool(b2)) -> Vbool(b1 || b2)
                    |   (Eq, Vbool(b1), Vbool(b2)) -> Vbool(b1 == b2)
                    |   (Eq, Vnum(n1), Vnum(n2)) -> Vbool(n1 == n2)
                    |   (Df, Vnum(n1), Vnum(n2)) -> Vbool(n1 != n2)
                    |   (Df, Vbool(b1), Vbool(b2)) -> Vbool(b1 != b2)
                    |   (Lt, Vnum(n1), Vnum(n2)) -> Vbool(n1 < n2)
                    |   (Le, Vnum(n1), Vnum(n2)) -> Vbool(n1 <= n2)
                    |   (Gt, Vnum(n1), Vnum(n2)) -> Vbool(n1 > n2)
                    |   (Ge, Vnum(n1), Vnum(n2)) -> Vbool(n1 >= n2)
                    |   _ -> raise NoValidOperation
        )
    |   Lam(varName, e) -> Vclos(varName, e, envi)
    |   Let(varName, e1, e2) -> 
        (
            let varValue = eval envi e1 in 
                eval ((varName, varValue)::envi) e2
        )
    |   Lrec(f, x, e1, e2) -> eval ((f, Vrclos(f, x, e1, envi))::envi) e2
    |   App(e1, e2) ->
    (
        let closure = eval envi e1 in
        let v' = eval envi e2 in
        match closure with
        |   Vclos(x, e, envi') -> eval ((x, v')::envi') e
        |   Vrclos(f, x, e, envi') -> eval ((x, v')::(f, closure)::envi') e
        |   _ -> raise NoRuleApplies
    )
    |   Cons(valueType, consTail) -> 
        (
            let v = eval envi valueType in
                Vcons(v, (eval envi consTail))
        )
    |   IsEmpty(e) ->
        (
            let v = eval envi e in
            match v with
            |   Vnil -> Vbool(true)
            |   Vcons(h, _) -> Vbool(false)
            |   _ -> raise NoValidList
        )
    |   Hd(e) ->
        (
            let v = eval envi e in
            match v with
            |   Vcons(h, _) -> h
            |   Vnil -> RRaise
            |   _ -> raise NoValidList
        )
    |   Tl(e) ->
        (
            let v = eval envi e in
            match v with
            |   Vcons(_, t) -> t
            |   Vnil -> RRaise
            |   _ -> raise NoValidList
        )
    |   Try(e1, e2) ->
        (
            let v1 = eval envi e1 in
            match v1 with
            |   RRaise -> eval envi e2
            |   _ -> v1
        )
    |   Raise -> RRaise;;
            
(* PRINT FINAL VALUE *)
let rec printEnvironment envi = 
    match envi with
    |   h::t -> 
        (
            match h with 
            |   (name, value) -> 
                (
                    Printf.printf "(%s, " name; printEvalRec value; Printf.printf ")"; 
                    match t with 
                    | h2::t2 -> Printf.printf ", "; printEnvironment t;
                    | _ -> ()
                )
        )
    |   _ -> ()

and printEvalRec v = 
    match v with
    |   Vnum(n)    -> Printf.printf "%d" n
    |   Vbool(b)   -> Printf.printf "%B" b
    |   Vnil       -> Printf.printf "NIL"
    |   Vcons(h, t) -> printEvalRec h; Printf.printf " :: "; printEvalRec t
    |   Vclos(varName, e, envi) -> 
            Printf.printf "<%s, " varName; 
            printEvalRec (eval envi e); 
            Printf.printf ", ["; 
            printEnvironment envi; 
            Printf.printf "]>" 
    |   Vrclos(f, x, e, envi) -> 
            Printf.printf "{ %s -> <%s, %s" f f x; 
            printEvalRec (eval envi e); 
            Printf.printf ", ["; 
            printEnvironment envi; 
            Printf.printf "]> }" 
    |   Vpair(f, s) -> 
            Printf.printf "("; printEvalRec f; Printf.printf ", "; printEvalRec s; Printf.printf ")"
    |   RRaise -> Printf.printf "RRaise";;



let printEval v = 
    Printf.printf "Value: ";
    printEvalRec v;
    Printf.printf "\n";;

let printEvalWithLabel label v =
    Printf.printf "%s => " (String.uppercase label);
    printEval v;;

(* Tests *)
let test00 = Bcte(true)
let test01 = Ncte(10)
let test02 = Binop(Sum, Ncte(10), Ncte(5))
let test03 = Binop(Eq, Ncte(10), Ncte(10))
let test04 = Cons(Ncte(10), Cons(Ncte(9), Cons(Ncte(8), Nil)))
let test05 = Lam("x", Ncte(10))
let test06 = Pair(Var("x"), Ncte(10))
let test07 = Var("x")
let test08 = If(Bcte(true), Ncte(0), Ncte(99))
let test09 = If(Bcte(false), Ncte(0), Ncte(99))
let test10 = If(Binop(Eq, Ncte(10), Ncte(10)), Ncte(10), Ncte(20))
let test11 = If(Binop(Df, Ncte(10), Ncte(10)), Ncte(10), Ncte(20))
let test12 = Let("x", Ncte(10), Binop(Sum, Var("x"), Var("x"))) (* let x = 10 in x + x *)
let test13 = App(Lam("x", Var("x")), Ncte(10)) (* "identity" (fn x => x)(10) *)
let test14 = App(Lam("x", Binop(Sum, Var("x"), Ncte(10))), Ncte(10)) (* (fn x => x + 10)(10) *)
let test15 = Let("sum_10", Lam("x", Binop(Sum, Var("x"), Ncte(10))), App(Var("sum_10"), Ncte(10))) (* let sum_10 = fn x => x + 10 in sum_10(10) *)
let test16 = Lrec("fat", "x", If(Binop(Le, Var("x"), Ncte(0)), Ncte(1), Binop(Mult, Var("x"), App(Var("fat"), Binop(Sub, Var("x"), Ncte(1))))), App(Var("fat"), Ncte(5))) 
                    (* let rec fat x = if x = 0 then 1 else x * fat(x - 1) in fat(5) *)

let test17 = Lrec("sumdown", "x",
                    If(Binop(Eq, Var("x"), Ncte(0)), Ncte(0), Binop(Sum, Var("x"),   App(Var("sumdown"),   Binop(Sub, Var("x"), Ncte(1))  )      ) ),
                    
                    App(Var("sumdown"), Ncte(5))) (* let rec sumdown x = if x = 0 then 0 else x + sumdown(x - 1) in sumdown 5;; *)

let test18 = IsEmpty(Nil) (* IsEmpty [] *)
let test19 = IsEmpty( Cons(Ncte(1), Cons(Ncte(2), Nil))) (* IsEmpty [1; 2] *)
let test20 = IsEmpty( Cons(Ncte(1), Nil)) (* IsEmpty [1] *)
let test21 = Hd( Cons(Ncte(1), Nil) ) (* Hd [1] *)
let test22 = Hd( Cons(Ncte(1), Cons(Ncte(2), Nil))) (* Hd [1; 2] *)
let test23 = Hd( Nil ) (* Hd Nil *)
let test24 = Tl( Cons(Ncte(1), Cons(Ncte(2), Nil)) ) (* Tl [1; 2] *)
let test25 = Tl( Cons(Ncte(1), Nil) ) (* Tl [1] *)
let test26 = Tl( Nil ) (* Tl Nil *)
let test27 = Try( Ncte(10), Nil) (* "BAD TYPE" try 10 with Nil *)
let test28 = Try( Tl(Nil), Cons(Ncte(10), Nil) ) (* "this will raise a condition" try Tl [] with [10] >>>>>>>>>>>>> WELL TYPED <<<<<<<<<<< *)
let test29 = Lrec("fat", "x", If(Binop(Le, Var("x"), Ncte(0)), Ncte(1), Binop(Mult, Var("x"), App(Var("fat"), Binop(Sub, Var("x"), Ncte(1))))), App(Var("fat"), Var("y")))
(*  
    environment [(y -> 5)]
    let rec fat x = if x = 0 then 1 else x * fat(x - 1) in fat y 
*)
let test30 = First(Pair(Ncte(10), Bcte(false)))
let test31 = Second(Pair(Ncte(10), Bcte(false)))

let v00 = eval [] test00;;
let v01 = eval [] test01;;
let v02 = eval [] test02;;
let v03 = eval [] test03;;
let v04 = eval [] test04;;
let v05 = eval [("y", Vbool(false))] test05;;
let v06 = eval [("x", Vnum(10))] test06;;
let v07 = eval [("x", Vnum(10))] test07;;
let v08 = eval [] test08;;
let v09 = eval [] test09;;
let v10 = eval [] test10;;
let v11 = eval [] test11;;
let v12 = eval [] test12;;
let v13 = eval [] test13;;
let v14 = eval [] test14;;
let v15 = eval [] test15;;
let v16 = eval [] test16;;
let v17 = eval [] test17;;
let v18 = eval [] test18;;
let v19 = eval [] test19;;
let v20 = eval [] test20;;
let v21 = eval [] test21;;
let v22 = eval [] test22;;
let v23 = eval [] test23;;
let v24 = eval [] test24;;
let v25 = eval [] test25;;
let v26 = eval [] test26;;
let v27 = eval [] test27;;
let v28 = eval [] test28;;
let v29 = eval [("y", Vnum(5))] test29;;
let v30 = eval [] test30;;
let v31 = eval [] test31;;

printEval v00;;
printEval v01;;
printEval v02;;
printEval v03;;
printEval v04;;
printEval v05;;
printEval v06;;
printEval v07;;
printEval v08;;
printEval v09;;
printEval v10;;
printEval v11;;
printEval v12;;
printEval v13;;
printEval v14;;
printEval v15;;
printEval v16;;
printEval v17;;
printEval v18;;
printEval v19;;
printEval v20;;
printEval v21;;
printEval v22;;
printEval v23;;
printEval v24;;
printEval v25;;
printEval v26;;
printEval v27;;
printEval v28;;
printEvalWithLabel "fatorial access y from environment" v29;;
printEvalWithLabel "first from (10, false)" v30;;
printEvalWithLabel "second from (10, false)" v31;;