open Typenames
open Unify
open Collect
open Applysubs


let typeinfer env p =
	let (t, c) = collect env p in
	let subs = unify [] c in
	applysubs subs t