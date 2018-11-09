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
    |   Nil -> Vnil
    |   Cons(valueType, consTail) -> 
        (
            let v = eval envi valueType in
                Vcons(v, (eval envi consTail))
        )
    |   Raise -> RRaise
    | _ -> raise NoRuleApplies;;
            
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
            | _ -> raise ExecutionError
        )
    |   _ -> ()

and printEvalRec v = 
    match v with
    |   Vnum(n)    -> Printf.printf "%d" n
    |   Vbool(b)   -> Printf.printf "%B" b
    |   Vnil       -> Printf.printf "NIL"
    |   Vcons(h, t) -> printEvalRec h; Printf.printf " -> "; printEvalRec t
    |   Vclos(varName, e, envi) -> 
            Printf.printf "<%s, " varName; 
            printEvalRec (eval envi e); 
            Printf.printf ", ["; 
            printEnvironment envi; 
            Printf.printf "]>" 
    |   Vpair(f, s) -> 
            Printf.printf "("; printEvalRec f; Printf.printf ", "; printEvalRec s; Printf.printf ")"
    |   _ -> ();;



let printEval v = 
    Printf.printf "Value: ";
    printEvalRec v;
    Printf.printf "\n";;

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