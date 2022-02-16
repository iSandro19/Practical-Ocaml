(* Título:  Práctica 9 - Árboles (ejercicio 2)*)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

open Bin_tree;;

let rec insert_tree f t x =
  match x with
      Empty -> Node (t, Empty, Empty)
    | Node (root, left, right) ->
      if (f t root)
        then Node (root, (insert_tree f t left), right)
      else Node (root, left, (insert_tree f t right));;

let sort f l =
  in_order (List.fold_left (fun x a -> insert_tree f a x) Empty l);;