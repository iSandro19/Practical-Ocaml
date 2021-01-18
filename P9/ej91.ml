(* Título:  Práctica 9 - Árboles (ejercicio 1)*)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

open Bin_tree;;

let rec fold_tree f a t = 
  match t with
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;

let sum t = fold_tree (fun x l r -> x + l + r) 0 t;;

let prod t = fold_tree (fun x l r -> x *. l *. r) 1.0 t;;

let num_nodes t = fold_tree (fun x l r -> 1 + l + r) 0 t;;

let in_order t = fold_tree (fun x l r -> l @ x::r) [] t;;

let mirror t = fold_tree (fun x l r -> Node (x, r, l)) Empty t;;

let rec prod2 = function
    Empty -> 1.0
  | Node (r, lb, rb) -> 
    if r = 0.
      then 0.
    else r *. (prod lb) *. (prod rb);;