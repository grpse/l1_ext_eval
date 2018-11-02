type variable = string

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
    |   Ge    (* >= *)
     
type uop = Not

type expr = 
        Ncte of int 
    |   Bcte of bool
    |   Binop of bop * expr * expr
    |   Unop of uop * expr
    |   Pair of expr * expr  
    |   If of expr * expr * expr 
    |   Var of variable 
    |   App of expr * expr 
    |   Lam of variable * expr
    |   Let of variable * expr * expr
    |   Lrec of variable *  variable * expr * expr
    |   Nil
    |   Cons of expr * expr
    |   IsEmpty of expr
    |   Hd of expr
    |   Tl of expr
    |   Raise
    |   Try of expr * expr
        
type result =
        Vnum of int 
    |   Vbool of bool 
    |   Vpair of result * result
    |   Vnil
    |   Vcons of result * result 
    |   Vclos of variable * expr * env
    |   Vrclos of variable * variable * expr * env
    |   RRaise
and
    env = (variable * result) list

(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies

(* BIG STEP L1 *)
let rec step env e = 
    match (env, e) with
    
    |   Ncte(n) -> Vnum(n)
    |   Bcte(b) -> Vbool(b)
    |   Nil -> Vnil
    |   Pair(e1, e2) -> Vpair(e1, e2)
    |   If (e1, e2, e3) −> 
    |   If (TmFalse, t2, t3) −> t3
    |   If (t1, t2, t3) −>
            let t1' = step (env, t1) in If (t1', t2, t3)
    |   Binop (e
            
            
(* Implementacao de EVAL *)
let rec eval (env, t) =
    try let t' = step (env, t)
        in eval (env, t')
    with NoRuleApplies −> t
    
(* Tests *)