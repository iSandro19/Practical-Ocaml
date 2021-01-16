(* Título:  Práctica 9 - Árboles              *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

let t = Node (3,
                Node (8, Empty, Empty),
                Node (2,
                        Node (5, Empty, Empty),
                        Node (1, Empty, Empty)));;

let rec fold_tree f a t = 
  match t with
    Empty -> a |
    Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;

let rec sum = function
    Empty -> 0
  | Node (r, lb, rb) -> r + (sum lb) + (sum rb);;

let rec prod = function
    Empty -> 1.0
  | Node (r, lb, rb) -> r *. (prod lb) *. (prod rb);;

let rec prod2 = function
    Empty -> 1.0
  | Node (r, lb, rb) -> r *. (prod lb) *. (prod rb);;

let rec num_nodes = function
    Empty -> 0
  | Node (_, lb, rb) -> 1 + num_nodes lb + num_nodes rb;;

let rec in_order = function
    Empty -> []
  | Node (r, lb, rb) -> in_order lb @ (r::in_order rb);;

let rec mirror = function
    Empty ->  Empty
  | Node (x, l, r) -> Node (x, mirror r, mirror l);;