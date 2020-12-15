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
    | h::[] -> if h > i then h else i
    | h::t ->
      if h > i
        then aux h t
      else aux i t
  in aux (List.hd l) l;;

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

let append l1 l2 =
  let rec aux l l1 l2 =
    match (l1, l2) with
    | ([], []) -> List.rev l
    | (h1::t1, []) -> l1
    | ([], h2::t2) -> aux (h2::l) [] t2
    | (h1::t1, h2::t2) -> aux (h1::l) t1 l2
  in aux [] l1 l2;;

let map f l = 
  let rec aux r l =
    match l with
    | [] -> List.rev r
    | h::t -> aux ((f h)::r) t
  in aux [] l;;

let power x y =
  let rec innerpower y k =
    if y = 0 then k
    else innerpower (y-1) (k*x)
  in
    if y >= 0 then innerpower y 1
    else invalid_arg "power";;

let incseg l =
  let rec aux r l =
    match (r, l) with
    | ([], []) -> r
    | ([], h1::h2::t) -> aux (h1::[]) (h2::t)
    | (hr::tr, []) -> List.rev r
    | ([], hl::[]) -> hl::r
    | (hr::tr, hl::tl) -> aux ((hr + hl)::r) tl
  in aux [] l;;

let rec last l =
  match l with
    [] -> failwith "last"
  | x::[] -> x
  | h::t -> last t;;
    
let remove x l =
  let rec aux r l cnt =
    match l with
      [] -> List.rev r
    | h::t ->
      if h = x && cnt = 0 then aux r t (cnt+1)
      else aux (h::r) t cnt
  in aux [] l 0;;

let insert x l =
  let rec aux bf af =
    match af with
      [] -> List.rev (x::bf)
    | h::t ->
      if x <= h then List.rev_append bf (x::af)
      else aux (h::bf) t
  in aux [] l;;

let insert_gen f x l =
  let rec aux bf af =
    match af with
      [] -> List.rev (x::bf)
    | h::t ->
      if f x h then List.rev_append bf (x::af)
      else aux (h::bf) t
  in aux [] l;;