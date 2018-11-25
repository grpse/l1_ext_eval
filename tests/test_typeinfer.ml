open Testexpressions
open Collect
open Typenames
open Unify
open Applysubs
open Typeinfer

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

printEquations ty00;;
printEquations ty01;;
printEquations ty02;;
printEquations ty03;;
printEquations ty04;;
printEquations ty05;;
printEquations ty06;;
printEquations ty07;;
printEquations ty08;;
printEquations ty09;;
printEquations ty10;;
printEquations ty11;;
printEquations ty12;;
printEquations ty13;;
printEquations ty14;;
printEquations ty15;;
printEquations ty16;;
printEquations ty17;;
printEquations ty18;;
printEquations ty19;;
printEquations ty20;;
printEquations ty21;;
printEquations ty22;;
printEquations ty23;;
printEquations ty24;;
printEquations ty25;;
printEquations ty26;;
printEquations ty27;;
printEquations ty28;;
printEquations ty29;;
printEquations ty30;;
