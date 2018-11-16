#use "type_names.ml";;

exception NoCollectableType;;
exception NoValidBinaryOperation;;
exception NotDeclaredIdentifier of string;;

let rec lookup tenvi var = 
    match tenvi with
    | 	tenvHead::tenvTail -> 
        (

            match tenvHead with 
            | (varName, varValue) -> 
                if varName = var
                    then varValue 
                else 
                    lookup tenvTail var
		)
	|	_ -> raise (NotDeclaredIdentifier var)
;;

let nextTypeVar x = "X" ^ string_of_int(x) ;;
let createTypeVar xn = Tx(nextTypeVar xn);;
    
let rec collect env e = 

	let rec collectWithTypeVarNumber env e varNumber =

		match e with
		|	Ncte(n) -> (Tint, [])
		|	Bcte(b) -> (Tbool, [])
		|	Binop(op, e1, e2) -> 
			(
				let (t1, c1) = collectWithTypeVarNumber env e1 varNumber in
				let (t2, c2) = collectWithTypeVarNumber env e2 varNumber in
				match (op, e1, e1) with
				|	(Sum,  _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(Sub,  _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(Div,  _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(Mult, _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(Gt,   _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(Lt,   _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(Ge,   _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(Le,   _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(Eq,   _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
				|	(And,  _, _) -> (Tbool, c1 @ c2 @ [(t1, Tbool); (t2, Tbool)])
				|	(Or,   _, _) -> (Tbool, c1 @ c2 @ [(t1, Tbool); (t2, Tbool)])
				|	_ -> raise NoValidBinaryOperation
			)

		|	Unop (op, e1) -> 
			(
				let (t, c) = collectWithTypeVarNumber env e1 varNumber in
				match (op, e1) with
				|	(Not, _) -> (Tbool, c @ [(t, Tbool)])
			)
		
		|	If(e1, e2, e3) ->
			(
				let (t1, c1) = collectWithTypeVarNumber env e1 varNumber in
				let (t2, c2) = collectWithTypeVarNumber env e2 varNumber in
				let (t3, c3) = collectWithTypeVarNumber env e3 varNumber in
					(t2, c1 @ c2 @ c3 @ [(t1, Tbool); (t2, t3)])
			)
		|	Pair(e1, e2) ->
			(
				let (t1, c1) = collectWithTypeVarNumber env e1 varNumber in
				let (t2, c2) = collectWithTypeVarNumber env e2 varNumber in
					(Tpair(t1, t2), c1 @ c2)
			)
		|	Let(x, e1, e2) ->
			(
				let (t1, c1) = collectWithTypeVarNumber env e1 varNumber in
				let (t2, c2) = collectWithTypeVarNumber ((x, t1)::env) e2 (varNumber + 1) in

					(t2, c1 @ c2 @ [((createTypeVar varNumber), t1)])
			)
		|	Var(x) ->
			(
				let t = lookup env x in
					(t, [])
			)
		|	Nil ->
			(
				let t = createTypeVar varNumber in
					(Tlist(t), [])
			)
		|	_ -> raise NoValidBinaryOperation

	in
		collectWithTypeVarNumber env e 1
;;

let rec printTypeEquationsTail typeEquations = 
	Printf.printf " -> ";
	match typeEquations with
	|	h::t ->	
		(
			Printf.printf 
			(
				match h with
				|	(Tint, Tint) -> "(Tint = Tint)"
				|	(Tbool, Tbool) -> "(Tbool = Tbool)"
				|	_ -> raise NoCollectableType
			); 
			printTypeEquationsTail t;
		)		
	|	[] -> Printf.printf "[]"
	
and printTypeEquations tequ = 
	match tequ with
	|	(t, equations) ->
		(
			Printf.printf 
			(
				match t with
				|	Tint -> "Tint "
				|	Tbool -> "Tbool"
				|	_ -> raise NoCollectableType
			);
			printTypeEquationsTail equations;
		)
;;

let rec printEquations typeEquations = 
	Printf.printf "Equation: ";
	printTypeEquations typeEquations;
	Printf.printf "\n"
;;

let test00 = Ncte(0)
let test01 = Bcte(false)
let test02 = Unop(Not, Bcte(true))

let v00 = collect [] test00;;
let v01 = collect [] test01;;
let v02 = collect [] test02;;

printEquations v00;;
printEquations v01;;
printEquations v02;;

