(* Título:  Práctica 9 - Árboles              *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

let t = Node (3, Node (8, Empty, Empty),
								 Node (2, Node (5, Empty, Empty),
													Node (1, Empty, Empty)));;

let rec fold_tree f a t = 
  match t with
    Empty -> a |
    Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;