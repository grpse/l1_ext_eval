open Typenames

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