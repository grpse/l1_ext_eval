open Typenames
open Unify
open Collect
open Applysubs


let typeinfer env P =
	let 
		(T, C) = collect env P
	in
		let subs = unify [] C
	in
		applysubs subs T