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

    
let rec collect env e = 
	match e with
		Ncte(n) -> (Tint, [])
	|	Bcte(b) -> (Tbool, [])
	|	Binop(op, e1, e2) -> 
		(
			let (t1, c1) = collect env e1 in
			let (t2, c2) = collect env e2 in
			match (op, e1, e1) with
				(Sum,  _, _) -> (Tint, c1 @ c2 @ [(t1, Tint); (t2, Tint)])
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
			let (t, c) = collect env e1 in
			match (op, e1) with
				(Not, _) -> (Tbool, c @ [(t, Tbool)])
		)
	
	|	If(e1, e2, e3) ->
		(
			let (t1, c1) = collect env e1 in
			let (t2, c2) = collect env e2 in
			let (t3, c3) = collect env e3 in
				(t2, c1 @ c2 @ c3 @ [(t1, Tbool); (t2, t3)])
		)
	|	_ -> raise NoValidBinaryOperation
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
;;

let rec printTypeEquations tequ = 
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

