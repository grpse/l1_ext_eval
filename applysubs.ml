open Typenames
open Util

exception SyntaxError

let rec lookupForType (subs : clist) var = 
	match subs with
	|	subsHead::subsTail -> 
		(
			match subsHead with 
			| (Tx(varName), varValue) -> 
				(
					(*(Printf.printf "%s\n" (typeToString varValue));*)
					if varName = var
						then  varValue
					else
						(
							match varValue with
							|	Tfn(t1, Tx(rname)) -> 
								(
									(*(Printf.printf "%s\n" (typeToString t1));*)
									if rname = var 
										then t1
									else 
										lookupForType subsTail var
								)
							|	Tfn(Tx(rname), t1) ->
								(
									(*(Printf.printf "%s\n" (typeToString t1));*)
									if rname = var 
									then t1
									else 
									lookupForType subsTail var
								) 
							|	_ -> varValue
						)
						
				)
			|	_ -> raise SyntaxError
		)
|	_ -> Tx(var)


let rec applysubs (subs : clist) t =
	match t with
		| Tint -> Tint
		| Tbool -> Tbool
		| Tfn(t1, t2) -> (*Printf.printf "(%s = %s)\n" (typeToString t1) (typeToString t2);*)
										 Tfn(applysubs subs t1, applysubs subs t2)
		| Tpair(t1, t2) -> Tpair(applysubs subs t1, applysubs subs t2)
		| Tlist(ty) -> Tlist(applysubs subs ty)
		| Tx(x) ->  
			(*printTypeEquationsTail subs; Printf.printf " [x = %s] " x; Printf.printf " [t = %s] " (typeToString t); Printf.printf "\n";*)
			try lookupForType subs x with SyntaxError -> Tx(x)
