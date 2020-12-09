(* Título:  Práctica 6 - Recursividad terminal*)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

let suml l =
  List.fold_left (+) 0 l;;

let maxl l =
  let rec aux i l =
    match l with
    | [] -> raise (Failure "maxl")
    | h::[] -> h
    | h::t ->
      if h > i
        then aux h t
      else aux i t
  in aux 0 l;;

let to0from n =
  let rec aux n l =
    if n < 0
      then List.rev l
    else aux (n-1) (n::l)
  in aux n [];;

let rec fromto m n =
  let rec aux n l =
    if n < m
      then l
    else aux (n-1) (n::l)
  in aux n [];;

let from1to n =
  let rec aux n l =
    if n < 1
      then l
    else aux (n-1) (n::l)
  in aux n [];;

let append = List.append;;

let append' l1 l2 = 

let map = List.map;;

let map f l = 

let power x y =
  let rec innerpower y k =
    if y = 0 then k
    else innerpower (y-1) (k*x)
  in
    if y >= 0 then innerpower y 1
    else invalid_arg "power";;

let incseg l =
  List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;

let incseg ' l =

let rec remove x = function
  [] -> []
| h::t -> if x = h then t
          else h::remove x t;;

let remove' x l =
  let rec aux r l =
    match l with
      [] -> List.rev r
    | h::t ->
      if h = x then aux r t
      else aux (h::r) t
  in aux [] l;;

let insert x l =
  let rec aux bf af =
    match af with
      [] -> List.rev (x::bf)
    | h::t ->
      if x <= h then List.rev_append bf (x::af)
      else aux (h::bf) t
  in aux [] l;;

let rec insert_gen f x l = match l with
  [] -> [x]
| h::t -> if f x h then x::l
          else h::insert_gen f x t;;

let insert_gen f x l =
  