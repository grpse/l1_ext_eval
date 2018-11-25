open Typenames

exception SyntaxError (* of string *)

let rec lookupForType (subs : clist) var = 
	match subs with
	| 	subsHead::subsTail -> 
			(

					match subsHead with 
					| (varName, varValue) -> 
						(
							if varName = Tx(var)
								then varValue
							else 
								lookupForType subsTail var
						)
	)
|	_ -> raise (NotDeclaredIdentifier var)


let rec applysubs (subs : clist) t =
	match t with
		| Tint -> Tint
		| Tbool -> Tbool
		| Tfn(t1, t2) -> Tfn(applysubs subs t1, applysubs subs t2)
		| Tpair(t1, t2) -> Tpair(applysubs subs t1, applysubs subs t2)
		| Tlist(ty) -> Tlist(applysubs subs ty)
		| Tx(x) -> try lookupForType subs x with SyntaxError -> Tx(x)
