open Testexpressions
open Collect
open Typenames
open Unify


let (a00, v00) = collect [] test00;;
let s00 = unify [] v00;;

let (a01, v01) = collect [] test01;;
let s01 = unify [] v01;;

let (a02, v02) = collect [] test02;;
let s02 = unify [] v02;;

let (a03, v03) = collect [] test03;;
let s03 = unify [] v03;;

let (a04, v04) = collect [] test04;;
let s04 = unify [] v04;;

let (a05, v05) = collect [("y", Tbool)] test05;;
let s05 = unify [] v05;;

let (a06, v06) = collect [("x", Tint)] test06;;
let s06 = unify [] v06;;

let (a07, v07) = collect [("x", Tint)] test07;;
let s07 = unify [] v07;;

let (a08, v08) = collect [] test08;;
let s08 = unify [] v08;;

let (a09, v09) = collect [] test09;;
let s09 = unify [] v09;;

let (a10, v10) = collect [] test10;;
let s10 = unify [] v10;;

let (a11, v11) = collect [] test11;;
let s11 = unify [] v11;;

let (a12, v12) = collect [] test12;;
let s12 = unify [] v12;;

let (a13, v13) = collect [] test13;;
let s13 = unify [] v13;;

let (a14, v14) = collect [] test14;;
let s14 = unify [] v14;;

let (a15, v15) = collect [] test15;;
let s15 = unify [] v15;;

let (a16, v16) = collect [] test16;;
let s16 = unify [] v16;;

let (a17, v17) = collect [] test17;;
let s17 = unify [] v17;;

let (a18, v18) = collect [] test18;;
let s18 = unify [] v18;;

let (a19, v19) = collect [] test19;;
let s19 = unify [] v19;;

let (a20, v20) = collect [] test20;;
let s2 = unify [] v20;;

let (a21, v21) = collect [] test21;;
let s21 = unify [] v21;;

let (a22, v22) = collect [] test22;;
let s22 = unify [] v22;;

let (a23, v23) = collect [] test23;;
let s23 = unify [] v23;;

let (a24, v24) = collect [] test24;;
let s24 = unify [] v24;;

let (a25, v25) = collect [] test25;;
let s25 = unify [] v25;;

let (a26, v26) = collect [] test26;;
let s26 = unify [] v26;;

let (a27, v27) = collect [] test27;;
let s27 = unify [] v27;;

let (a28, v28) = collect [] test28;;
let s28 = unify [] v28;;

let (a29, v29) = collect [("y", Tint)] test29;;
let s29 = unify [] v29;;

let (a30, v30) = collect [("y", Tint)] test30;;
let s30 = unify [] v30;;

printEquations s00;;
printEquations s01;;
printEquations s02;;
printEquations s03;;
printEquations s04;;
printEquations s05;;
printEquations s06;;
printEquations s07;;
printEquations s08;;
printEquations s09;;
printEquations s10;;
printEquations s11;;
printEquations s12;;
printEquations s13;;
printEquations s14;;
printEquations s15;;
printEquations s16;;
printEquations s17;;
printEquations s18;;
printEquations s19;;
printEquations s20;;
printEquations s21;;
printEquations s22;;
printEquations s23;;
printEquations s24;;
printEquations s25;;
printEquations s26;;
printEquations s27;;
printEquations s28;;
printEquations s29;;
printEquations s30;;