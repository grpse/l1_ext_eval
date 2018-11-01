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
          
          
(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t = match t with
    (* CASO IF ( t1 , t2 , t3 ) *)
        If (TmTrue, t2, t3 ) −> t2 (* regra E−IfTrue *)
    |   If (TmFalse, t2, t3 ) −> t3 (* regra E−False *)
    |   If (t1, t2, t3) −> (* regra E−If *)
        let t1' = step t1 in
            If ( t1', t2 , t3 )
            (* CASO SUCC ( t1 ) *)
            