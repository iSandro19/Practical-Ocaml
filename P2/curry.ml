(* Título:  Práctica 2 - curry                *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

let curry f a b = f (a, b);;    (*  Función curry  *)

let uncurry f (a, b) = f a b;;  (* Función uncurry *)

uncurry (+);;
(* - : int * iny -> int = <fun> *)

let sum = (uncurry (+));;
(* val sum : int * int -> int = <fun> *)

(* sum 1;;
   Error de tipo: se espera un par de enteros int * int
   y solo se pasa uno.
 *)

sum(2, 1);;
(* - : int = 3 *)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(* val g : int -> int -> int = <fun> *)

(* g (2,5);;
   Error de tipo: se espera un entero int 
   y se pasa un par de enteros int * int.
 *)

let h = g 2;;
(* val h : int -> int = <fun> *)

h 1, h 2, h 3;;
(* - : int * int * int = (7, 10, 13)
   Importante! ---> Se hace (2*2 + 3*1) & (2*2 + 3*2) & (2*2 + 3*3)
   El valor que cambia es el segundo de p, que es lo que se pasa a h.
 *)