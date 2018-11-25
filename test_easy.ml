
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
    |   Try of expr * expr
        
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
    env = (variable * result) list

type typename =
    |   Tx of variable
    |   Tint
    |   Tbool
    |   Tpair of typename * typename
    |   Tfn of typename * typename
    |   Tlist of typename
and
    tenv = (variable * typename) list

type clist = (typename * typename) list

exception NoCollectableType
exception NoValidBinaryOperation
exception NotDeclaredIdentifier of string
exception NoValidExpression
exception SyntaxError (* of string *)

(*
let rec lookup tenvi (var : variable) = 
	match tenvi with
	| [] -> raise (NotDeclaredIdentifier var)
	| (varName, varValue)::tenvTail -> 
		if varName = var
			then varValue 
	| tenvHead::tenvTail -> lookup tenvTail var
*)

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


(* Procura a ocorrencia de X em T*)
let rec occurs x t =
  match t with
    | Tx y -> x = Tx(y)
    | Tfn(t1, t2) -> (occurs t1 x || occurs t2 x)
    | Tpair(t1, t2) -> (occurs t1 x || occurs t2 x)
    | Tlist(tl) -> occurs tl x
    | _ -> false

let rec unify (subs : tenv) (c : clist) =
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
          then unify (subs @ [(x, t)]) restC
        else raise SyntaxError (* evitar circularidade *)
    | (_, _) :: restC -> raise SyntaxError
    (*| _ -> raise SyntaxError*)


let rec applysubs (subs : tenv) (t : typename) =
	match t with
		| Tint -> Tint
		| Tbool -> Tbool
		| Tfn(t1, t2) -> Tfn(applysubs subs t1, applysubs subs t2)
		| Tpair(t1, t2) -> Tpair(applysubs subs t1, applysubs subs t2)
		| Tlist(ty) -> Tlist(applysubs subs ty)
		| Tx(x) -> 
			try lookup subs x
			with SyntaxError -> Tx(x)


let typeinfer env program =
	let (ty, constr) = collect env program in
	let subs = unify [] constr in
	applysubs subs ty


(* Tests *)
let test00 = Bcte(true)
let test01 = Ncte(10)
let test02 = Binop(Sum, Ncte(10), Ncte(5))
let test03 = Binop(Eq, Ncte(10), Ncte(10))
let test04 = Cons(Ncte(10), Cons(Ncte(9), Cons(Ncte(8), Nil)))
let test05 = Lam("x", Ncte(10))
let test06 = Pair(Var("x"), Ncte(10))
let test07 = Var("x")
let test08 = If(Bcte(true), Ncte(0), Ncte(99))
let test09 = If(Bcte(false), Ncte(0), Ncte(99))
let test10 = If(Binop(Eq, Ncte(10), Ncte(10)), Ncte(10), Ncte(20))
let test11 = If(Binop(Df, Ncte(10), Ncte(10)), Ncte(10), Ncte(20))
let test12 = Let("x", Ncte(10), Binop(Sum, Var("x"), Var("x"))) (* let x = 10 in x + x *)
let test13 = App(Lam("x", Var("x")), Ncte(10)) (* "identity" (fn x => x)(10) *)
let test14 = App(Lam("x", Binop(Sum, Var("x"), Ncte(10))), Ncte(10)) (* (fn x => x + 10)(10) *)
let test15 = Let("sum_10", Lam("x", Binop(Sum, Var("x"), Ncte(10))), App(Var("sum_10"), Ncte(10))) (* let sum_10 = fn x => x + 10 in sum_10(10) *)
let test16 = Lrec("fat", "x", If(Binop(Le, Var("x"), Ncte(0)), Ncte(1), Binop(Mult, Var("x"), App(Var("fat"), Binop(Sub, Var("x"), Ncte(1))))), App(Var("fat"), Ncte(5))) 
                    (* let rec fat x = if x = 0 then 1 else x * fat(x - 1) in fat(5) *)

let test17 = Lrec("sumdown", "x",
                    If(Binop(Eq, Var("x"), Ncte(0)), Ncte(0), Binop(Sum, Var("x"),   App(Var("sumdown"),   Binop(Sub, Var("x"), Ncte(1))  )      ) ),
                    
                    App(Var("sumdown"), Ncte(5))) (* let rec sumdown x = if x = 0 then 0 else x + sumdown(x - 1) in sumdown 5;; *)

let test18 = IsEmpty(Nil) (* IsEmpty [] *)
let test19 = IsEmpty( Cons(Ncte(1), Cons(Ncte(2), Nil))) (* IsEmpty [1; 2] *)
let test20 = IsEmpty( Cons(Ncte(1), Nil)) (* IsEmpty [1] *)
let test21 = Hd( Cons(Ncte(1), Nil) ) (* Hd [1] *)
let test22 = Hd( Cons(Ncte(1), Cons(Ncte(2), Nil))) (* Hd [1; 2] *)
let test23 = Hd( Nil ) (* Hd Nil *)
let test24 = Tl( Cons(Ncte(1), Cons(Ncte(2), Nil)) ) (* Tl [1; 2] *)
let test25 = Tl( Cons(Ncte(1), Nil) ) (* Tl [1] *)
let test26 = Tl( Nil ) (* Tl Nil *)
let test27 = Try( Ncte(10), Nil) (* "BAD TYPE" try 10 with Nil *)
let test28 = Try( Tl(Nil), Cons(Ncte(10), Nil) ) (* "this will raise a condition" try Tl [] with [10] >>>>>>>>>>>>> WELL TYPED <<<<<<<<<<< *)
let test29 = Lrec("fat", "x", If(Binop(Le, Var("x"), Ncte(0)), Ncte(1), Binop(Mult, Var("x"), App(Var("fat"), Binop(Sub, Var("x"), Ncte(1))))), App(Var("fat"), Var("y")))
(*  
    environment [(y -> 5)]
    let rec fat x = if x = 0 then 1 else x * fat(x - 1) in fat y 
*)
let test30 = Let("square", Lam("x", 
                    Binop(Mult, Var("x"), Var("x"))
                ), 
                App(Var("square"), Var("y")))
let test31 = First(Pair(Ncte(10), Bcte(false)))
let test32 = Second(Pair(Ncte(10), Bcte(false)))



let ty00 = typeinfer [] test00;;
let ty01 = typeinfer [] test01;;
let ty02 = typeinfer [] test02;;
let ty03 = typeinfer [] test03;;
let ty04 = typeinfer [] test04;;
let ty05 = typeinfer [("y", Tbool)] test05;;
let ty06 = typeinfer [("x", Tint)] test06;;
let ty07 = typeinfer [("x", Tint)] test07;;
let ty08 = typeinfer [] test08;;
let ty09 = typeinfer [] test09;;
let ty10 = typeinfer [] test10;;
let ty11 = typeinfer [] test11;;
let ty12 = typeinfer [] test12;;
let ty13 = typeinfer [] test13;;
let ty14 = typeinfer [] test14;;
let ty15 = typeinfer [] test15;;
let ty16 = typeinfer [] test16;;
let ty17 = typeinfer [] test17;;
let ty18 = typeinfer [] test18;;
let ty19 = typeinfer [] test19;;
let ty20 = typeinfer [] test20;;
let ty21 = typeinfer [] test21;;
let ty22 = typeinfer [] test22;;
let ty23 = typeinfer [] test23;;
let ty24 = typeinfer [] test24;;
let ty25 = typeinfer [] test25;;
let ty26 = typeinfer [] test26;;
let ty27 = typeinfer [] test27;;
let ty28 = typeinfer [] test28;;
let ty29 = typeinfer [("y", Tint)] test29;;
let ty30 = typeinfer [("y", Tint)] test30;;
