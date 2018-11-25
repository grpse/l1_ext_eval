open Typenames

let rec printTypeEquationsTail typeEquations = 
	match typeEquations with
	|	h::t ->	
		(
			(
				match h with
				|	(t1, t2) -> Printf.printf "(%s = %s)" (typeToString t1) (typeToString t2);
			);
			(
				match t with
				|	(t1, t2)::_ -> Printf.printf ", ";
				|	[] -> Printf.printf " "
			);
			printTypeEquationsTail t;
		)		
	|	[] -> Printf.printf "[] "

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
			Printf.printf "(%s, [" (typeToString t);
			printTypeEquationsTail equations;
			Printf.printf "])";
		)

let equationCounter = ref 0;;

let rec printEquations typeEquations = 
	equationCounter := !equationCounter + 1;
	Printf.printf "Equation %d: " !equationCounter;
  printTypeEquations typeEquations;
  Printf.printf "\n";
  