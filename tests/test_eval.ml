open Eval
open Testexpressions
open Typenames

let v00 = eval [] test00;;
let v01 = eval [] test01;;
let v02 = eval [] test02;;
let v03 = eval [] test03;;
let v04 = eval [] test04;;
let v05 = eval [("y", Vbool(false))] test05;;
let v06 = eval [("x", Vnum(10))] test06;;
let v07 = eval [("x", Vnum(10))] test07;;
let v08 = eval [] test08;;
let v09 = eval [] test09;;
let v10 = eval [] test10;;
let v11 = eval [] test11;;
let v12 = eval [] test12;;
let v13 = eval [] test13;;
let v14 = eval [] test14;;
let v15 = eval [] test15;;
let v16 = eval [] test16;;
let v17 = eval [] test17;;
let v18 = eval [] test18;;
let v19 = eval [] test19;;
let v20 = eval [] test20;;
let v21 = eval [] test21;;
let v22 = eval [] test22;;
let v23 = eval [] test23;;
let v24 = eval [] test24;;
let v25 = eval [] test25;;
let v26 = eval [] test26;;
let v27 = eval [] test27;;
let v28 = eval [] test28;;
let v29 = eval [("y", Vnum(5))] test29;;
let v30 = eval [("y", Vnum(5))] test30;;
let v31 = eval [] test31;;
let v32 = eval [] test32;;

printEval v00;;
printEval v01;;
printEval v02;;
printEval v03;;
printEval v04;;
printEval v05;;
printEval v06;;
printEval v07;;
printEval v08;;
printEval v09;;
printEval v10;;
printEval v11;;
printEval v12;;
printEval v13;;
printEval v14;;
printEval v15;;
printEval v16;;
printEval v17;;
printEval v18;;
printEval v19;;
printEval v20;;
printEval v21;;
printEval v22;;
printEval v23;;
printEval v24;;
printEval v25;;
printEval v26;;
printEval v27;;
printEval v28;;
printEvalWithLabel "fatorial access y from environment" v29;;
printEvalWithLabel "first from (10, false)" v30;;
printEvalWithLabel "second from (10, false)" v31;;
printEvalWithLabel "let square = fn x => x * x" v32;;