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

(* BIG STEP L1 *)
let rec eval envi e = 
    match e with
        Ncte(n) -> Vnum(n)
    |   Bcte(b) -> Vbool(b)
    |   Binop(op, e1, e2) -> (
                                let v1 = eval envi e1 in
                                let v2 = eval envi e2 in
                                    match (op, v1, v2) with 
                                        |   (Sum, Vnum(n1), Vnum(n2)) -> Vnum(n1 + n2)
                                        |   (Sub, Vnum(n1), Vnum(n2)) -> Vnum(n1 - n2)
                                        |   _ -> raise NoRuleApplies
                            )
    |   Let(name, e1, e2) -> (
                                let v = eval envi e1 
                                    in eval ((name, v)::envi) e2
                            )
    |   Nil -> Vnil
    | Raise -> RRaise
    | _ -> raise NoRuleApplies;;
            
(* PRINT FINAL VALUE *)
let rec printEvalRec v = 
    match v with
    | Vnum(n)    -> Printf.printf "%d " n
    | Vbool(b)   -> Printf.printf "%B " b
    | Vnil       -> Printf.printf "NIL "
    | Vcons(h, t) -> Printf.printf "List: "; printEvalRec h; printEvalRec t
    | _ -> ();;

let printEval v = 
    Printf.printf "Value: ";
    printEvalRec v;
    Printf.printf "\n";;

(* Tests *)
let test0 = Bcte(true)
let test1 = Ncte(10)
let test2 = Binop(Sum, Ncte(10), Ncte(5))

let v00 = eval [] test0;;
let v01 = eval [] test1;;
let v02 = eval [] test2;;

printEval v00;;
printEval v01;;
printEval v02;;