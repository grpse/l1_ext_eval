open Typenames

(* Tests *)
let test00 = Bcte(true)
let test01 = Ncte(10)
let test02 = Binop(Sum, Ncte(10), Ncte(5))
let test03 = Binop(Eq, Ncte(10), Ncte(10))
let test04 = Cons(Ncte(10), Cons(Ncte(9), Cons(Ncte(8), Nil)))
let test05 = Binop(Sum, Var("x"), Ncte(10))
let test06 = Pair(Var("x"), Ncte(10))
let test07 = Var("x")
let test08 = If(Bcte(true), Ncte(0), Ncte(99))
let test09 = If(Bcte(false), Ncte(0), Ncte(99))
let test10 = If(Binop(Eq, Ncte(10), Ncte(10)), Ncte(10), Ncte(20))
let test11 = If(Binop(Df, Ncte(10), Ncte(10)), Ncte(10), Ncte(20))
let test12 = Let("x", Ncte(10), Binop(Sum, Var("x"), Var("x"))) (* let x = 10 in x + x *)
let test13 = Lam("x", Var("x")) (* Let("f", Lam("x", Var("x")), App(Var("f"), Ncte(10))) *) (* "identity" (fn x => x)(10) *)
let test14 = Let("f", Lam("x", Binop(Sum, Var("x"), Ncte(10))), App(Var("f"), Ncte(10))) (* (fn x => x + 10)(10) *)
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
let test23 = Hd( Cons(Bcte(true), Nil) ) (* Hd Nil *)
let test24 = Tl( Cons(Ncte(1), Cons(Ncte(2), Nil)) ) (* Tl [1; 2] *)
let test25 = Tl( Cons(Ncte(1), Nil) ) (* Tl [1] *)
let test26 = Tl( Cons(Ncte(1), Cons(Ncte(2), Nil)) ) (* Tl Nil *)
let test27 = Try( Ncte(10), Ncte(1)) (* try 10 with 1 *)
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
let test31 = Lam("x", Lam("y", Var("x")))