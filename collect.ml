open Typenames

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

let nextTypeVar x = "X" ^ string_of_int(x)
let createTypeVar xn = Tx(nextTypeVar xn)
		
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
				|	(Sum,  _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Sub,  _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Div,  _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Mult, _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Gt,   _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Lt,   _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Ge,   _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Le,   _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Eq,   _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(Df,   _, _) -> (Tint,  c1 @ c2 @ [(t1, Tint);  (t2, Tint)])
				|	(And,  _, _) -> (Tbool, c1 @ c2 @ [(t1, Tbool); (t2, Tbool)])
				|	(Or,   _, _) -> (Tbool, c1 @ c2 @ [(t1, Tbool); (t2, Tbool)])
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
		|	Var(x) ->
			(
				let t = lookup env x in
				(t, [])
			)
		|	App(e1, e2) ->
			(
				let _X = createTypeVar varNumber in
				let (t1, c1) = collectWithTypeVarNumber env e1 (varNumber + 1) in
				let (t2, c2) = collectWithTypeVarNumber env e2 (varNumber + 2) in
				(_X, c1 @ c2 @ [(t1,Tfn(t2, _X))])
			)
		|	Lam(x, e) ->
			(
				let _X = createTypeVar varNumber in
				let (t, c) = collectWithTypeVarNumber ((x, _X)::env) e (varNumber + 1) in
				(Tfn(_X, t), c)
			)
		|	Let(x, e1, e2) ->
			(
				let _X = createTypeVar varNumber in
				let (t1, c1) = collectWithTypeVarNumber env e1 (varNumber + 1) in
				let (t2, c2) = collectWithTypeVarNumber ((x, _X)::env) e2 (varNumber + 2) in
				(t2, c1 @ c2 @ [(_X, t1)])
			)
		|	Lrec(f, x, e1, e2) ->
			(
				let _X = createTypeVar varNumber in 
				let _Y = createTypeVar (varNumber + 1) in
				let (t1, c1) = collectWithTypeVarNumber ((f, _X)::(x, _Y)::env) e1 (varNumber + 2) in
				let (t2, c2) = collectWithTypeVarNumber ((f, _X)::env) e2 (varNumber + 3) in
				(t2, c1 @ c2 @ [(_X,Tfn(_Y, t1))])
			)
		|	Nil ->
			(
				let t = createTypeVar varNumber in
				(Tlist(t), [])
			)
		|	Tl(e) -> 
			(
				let _X = createTypeVar varNumber in
				let (t, c) = collectWithTypeVarNumber env e (varNumber + 1) in
				(Tlist(_X), c @ [(t, Tlist(_X))])
			)
		|	Cons(e1, e2) ->
			(
				let (t1, c1) = collectWithTypeVarNumber env e1 varNumber in
				let (t2, c2) = collectWithTypeVarNumber env e2 varNumber in
				(t2, c1 @ c2 @ [(Tlist(t1), t2)])
			)
		|	IsEmpty(e) ->
			(
				let _X = createTypeVar varNumber in
				let (t, c) = collectWithTypeVarNumber env e (varNumber + 1) in
				(Tbool, c @ [(t, Tlist(_X))])
			)
		|	Hd(e) ->
			(
				let _X = createTypeVar varNumber in
				let (t, c) = collectWithTypeVarNumber env e (varNumber + 1) in
				(_X, c @ [(t, Tlist(_X))])
			)
		|	Raise ->
			(
				let _X = createTypeVar varNumber in
				(_X, [])
			)
		|	Try(e1, e2) ->
			(
				let (t1, c1) = collectWithTypeVarNumber env e1 varNumber in
				let (t2, c2) = collectWithTypeVarNumber env e2 varNumber in
				(t2, c1 @ c2 @ [(t1, t2)])
			)
		|	_ -> raise NoValidExpression

	in
		collectWithTypeVarNumber env e 1

let rec printTypeEquationsTail typeEquations = 
	match typeEquations with
	|	h::t ->	
		(
			match h with
			|	(t1, t2) -> Printf.printf "(%s = %s), " (typeToString t1) (typeToString t2);
			printTypeEquationsTail t;
		)		
	|	[] -> Printf.printf "[]"

and	typeToString t =
	match t with
	|	Tx(x) -> Printf.sprintf "Tx('%s')" x
	|	Tint -> "Tint"
	|	Tbool -> "Tbool"
	|	Tpair(t1, t2) -> Printf.sprintf "(%s, %s)" (typeToString t1) (typeToString t2)
	|	Tfn(t1, t2) -> Printf.sprintf "%s -> %s" (typeToString t1) (typeToString t2)
	|	Tlist(tl) -> Printf.sprintf "Tlist(%s)" (typeToString tl)

and printTypeEquations tequ = 
	match tequ with
	|	(t, equations) ->
		(
			Printf.printf "(%s, " (typeToString t);
			printTypeEquationsTail equations;
			Printf.printf ")";
		)

let rec printEquations typeEquations = 
	Printf.printf "Equation: ";
	printTypeEquations typeEquations;
	Printf.printf "\n"