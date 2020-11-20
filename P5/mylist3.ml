(* Título:  Práctica 5 - Listas opcional      *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

(* Función remove *)
let rec remove item l =
  match l with
    [] -> []
  | h::t ->
    if item = h
      then t
    else
      h::remove item t;;

(* Función remove_all *)
let remove_all item =
  let rec remove aux = function
    [] ->
      let rec rev rev_aux = function
        [] -> rev_aux
      | h::t -> rev (h::rev_aux) t in
    rev [] aux
  | h::t ->
    if item=h
      then remove aux t
    else remove (h::aux) t in
  remove [];;

(* Función ldif *)
let rec ldif l1 l2 =
  match l2 with
    [] ->  l1
  | h::t ->
    let rec remove aux = function
    | [] ->
      let rec rev rev_aux = function
        [] -> rev_aux
      | hr::tr -> rev (hr::rev_aux) tr in
      rev [] aux
    | hx::tx ->
      if hx = h
        then remove aux tx
      else remove (hx::aux) tx in
    ldif (remove [] l1) t;;

(* Función lprod *)
let rec lprod l1 l2 =
  match l1 with
    [] -> []
  | h::t ->
    let rec aux l =
      match l with
        [] -> []
      | n::r -> (h,n)::aux r in
    let rec append l1 l2 =
      match l1 with
        [] -> l2
      | h::t -> h::append t l2 in
      append (aux l2) (lprod t l2);;

let rec divide l =
  let rec aux i even odd = function
    [] ->
      let rec rev aux = function
          [] -> aux
        | h::t -> rev (h::aux) t in
      rev [] even, rev [] odd
  | h::t ->
    if (i mod 2) = 0
      then aux (i+1) (h::even) odd t
    else aux (i+1) even (h::odd) t in
  aux 0 [] [] l;;