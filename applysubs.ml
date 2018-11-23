open Typenames

exception SyntaxError (* of string *)

let rec applysubs (subs : tenv) t =
	match t with
		| Tint -> Tint
		| Tbool -> Tbool
		| Tfn(t1, t2) -> Tfn(applysubs subs t1, applysubs subs t2)
		| Tpair(t1, t2) -> Tpair(applysubs subs t1, applysubs subs t2)
		| Tlist(ty) -> Tlist(applysubs subs ty)
		| Tx(x) -> try lookup subs t with SyntaxError -> Tx(x)