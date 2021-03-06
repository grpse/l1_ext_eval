open Typenames
open Util

exception SyntaxError (* of string *)

(* Procura a ocorrencia de X em T*)
let rec occurs x t =
  match t with
    | Tx y -> x = t
    | Tfn(t1, t2) -> (occurs t1 x || occurs t2 x)
    | Tpair(t1, t2) -> (occurs t1 x || occurs t2 x)
    | Tlist(tl) -> occurs tl x
    | _ -> false

let rec unify (subs : clist) (c : clist) : clist =
  match c with
      [] -> subs
    | (Tint, Tint) :: restC -> unify subs restC
    | (Tbool, Tbool) :: restC -> unify subs restC
    | (Tlist(t1), Tlist(t2)) :: restC -> unify subs ((t1, t2) :: restC)
    | (Tfn(t1, t2), Tfn(t3, t4)) :: restC -> unify subs ((t1, t3) :: (t2, t4) :: restC)
    | (Tpair(t1, t2), Tpair(t3, t4)) :: restC -> unify subs ((t1, t3) :: (t2, t4) :: restC)
    | (Tx(x1), Tx(x2)) :: restC -> unify subs restC
    | (Tx(x), t) :: restC | (t, Tx(x)) :: restC -> (* se X não ocorre em T então 
                                                  Unify(σ@[(X, T)], {T /X}C) senão falha *)
        if not (occurs (Tx(x)) t)
          then unify (subs @ [(Tx(x), t)]) restC
        else raise SyntaxError; (* evitar circularidade *)
    | (_, _) :: restC -> printTypeEquationsTail c; raise SyntaxError