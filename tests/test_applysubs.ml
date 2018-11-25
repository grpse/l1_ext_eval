open Testexpressions
open Collect
open Typenames
open Unify
open Applysubs
open Util

let (a00, v00) = collect [] test00;;
let s00 = unify [] v00;;
let as00 = applysubs s00 a00;;

let (a01, v01) = collect [] test01;;
let s01 = unify [] v01;;
let as01 = applysubs s01 a01;;

let (a02, v02) = collect [] test02;;
let s02 = unify [] v02;;
let as02 = applysubs s02 a02;;

let (a03, v03) = collect [] test03;;
let s03 = unify [] v03;;
let as03 = applysubs s03 a03;;

let (a04, v04) = collect [] test04;;
let s04 = unify [] v04;;
let as04 = applysubs s04 a04;;

let (a05, v05) = collect [("y", Tbool)] test05;;
let s05 = unify [] v05;;
let as05 = applysubs s05 a05;;

(*
let (a06, v06) = collect [("x", Tint)] test06;;
let s06 = unify [] v06;;
let as06 = applysubs s06 a06;;

let (a07, v07) = collect [("x", Tint)] test07;;
let s07 = unify [] v07;;
let as07 = applysubs s07 a07;;

let (a08, v08) = collect [] test08;;
let s08 = unify [] v08;;
let as08 = applysubs s08 a08;;

let (a09, v09) = collect [] test09;;
let s09 = unify [] v09;;
let as09 = applysubs s09 a09;;

let (a10, v10) = collect [] test10;;
let s10 = unify [] v10;;
let as10 = applysubs s10 a10;;

let (a11, v11) = collect [] test11;;
let s11 = unify [] v11;;
let as11 = applysubs s11 a11;;

let (a12, v12) = collect [] test12;;
let s12 = unify [] v12;;
let as12 = applysubs s12 a12;;

let (a13, v13) = collect [] test13;;
let s13 = unify [] v13;;
let as13 = applysubs s13 a13;;

let (a14, v14) = collect [] test14;;
let s14 = unify [] v14;;
let as14 = applysubs s14 a14;;

let (a15, v15) = collect [] test15;;
let s15 = unify [] v15;;
let as15 = applysubs s15 a15;;

let (a16, v16) = collect [] test16;;
let s16 = unify [] v16;;
let as16 = applysubs s16 a16;;

let (a17, v17) = collect [] test17;;
let s17 = unify [] v17;;
let as17 = applysubs s17 a17;;

let (a18, v18) = collect [] test18;;
let s18 = unify [] v18;;
let as18 = applysubs s18 a18;;

let (a19, v19) = collect [] test19;;
let s19 = unify [] v19;;
let as19 = applysubs s19 a19;;

let (a20, v20) = collect [] test20;;
let s20 = unify [] v20;;
let as20 = applysubs s20 a20;;

let (a21, v21) = collect [] test21;;
let s21 = unify [] v21;;
let as21 = applysubs s21 a21;;

let (a22, v22) = collect [] test22;;
let s22 = unify [] v22;;
let as22 = applysubs s22 a22;;

let (a23, v23) = collect [] test23;;
let s23 = unify [] v23;;
let as23 = applysubs s23 a23;;

let (a24, v24) = collect [] test24;;
let s24 = unify [] v24;;
let as24 = applysubs s24 a24;;

let (a25, v25) = collect [] test25;;
let s25 = unify [] v25;;
let as25 = applysubs s25 a25;;

let (a26, v26) = collect [] test26;;
let s26 = unify [] v26;;
let as26 = applysubs s26 a26;;

let (a27, v27) = collect [] test27;;
let s27 = unify [] v27;;
let as27 = applysubs s27 a27;;

let (a28, v28) = collect [] test28;;
let s28 = unify [] v28;;
let as28 = applysubs s28 a28;;

let (a29, v29) = collect [("y", Tint)] test29;;
let s29 = unify [] v29;;
let as29 = applysubs s29 a29;;

let (a30, v30) = collect [("y", Tint)] test30;;
let s30 = unify [] v30;;
let as30 = applysubs s30 a30;;

printEquations (as00, []);;
printEquations (as01, []);;
printEquations (as02, []);;
printEquations (as03, []);;
printEquations (as04, []);;
printEquations (as05, []);;
printEquations (as06, []);;
printEquations (as07, []);;
printEquations (as08, []);;
printEquations (as09, []);;
printEquations (as10, []);;
printEquations (as11, []);;
printEquations (as12, []);;
printEquations (as13, []);;
printEquations (as14, []);;
printEquations (as15, []);;
printEquations (as16, []);;
printEquations (as17, []);;
printEquations (as18, []);;
printEquations (as19, []);;
printEquations (as20, []);;
printEquations (as21, []);;
printEquations (as22, []);;
printEquations (as23, []);;
printEquations (as24, []);;
printEquations (as25, []);;
printEquations (as26, []);;
printEquations (as27, []);;
printEquations (as28, []);;
printEquations (as29, []);;
printEquations (as30, []);;

*)