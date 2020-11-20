(* Título:  Primes			                      *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

let is_prime n = 
    let rec not_div_from d = 
	    d * d > n || (n mod d <> 0 && not_div_from (d+1)) in
    n > 1 && not_div_from 2;;
	
let rec next_prime n =
	if n < 2
		then 2
	else
		let n = (n+1) in 
	let rec prm d = 
		d*d > n || (n mod d <> 0 && prm (d+1)) in
	if prm n
		then n
	else
		next_prime (n+1);;

	
let rec prev_prime	n = (* n > 2 *)
	let n = pred n in
		if is_prime n
			then n
		else prev_prime n;;
	
	
let rec primes_between m n = 
	if m > n
		then []
	else
		if is_prime m
			then m :: primes_between (m+1) n
	else primes_between (m+1) n;;
	
	
let primes_till n = primes_between 2 n;;
