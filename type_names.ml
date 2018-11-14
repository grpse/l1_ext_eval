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

type typename =
    |   Tx of variable
    |   Tint
    |   Tbool
    |   Tpair of typename * typename
    |   Tfn of typename * typename
	|   Tlist of typename
and
	tenv = (variable * typename) list;;

type clist = (typename * typename) list;;


let nextTypeVar x =
	"X" ^ string_of_int(x)
;;

Printf.printf "%s\n" (nextTypeVar 1);;
Printf.printf "%s\n" (nextTypeVar 2);;
Printf.printf "%s\n" (nextTypeVar 3);;
