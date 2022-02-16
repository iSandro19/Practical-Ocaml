(* Título:  Práctica 2 - comp                 *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

let comp f g x = f (g x);;

let f = let square x = x * x in comp square ((+) 1);;
(* val f : int -> int -> <fun> *)

f 1, f 2, f 3;;
(* - : int * int * int = (4, 9, 16) *)

(* Si recogemos la función uncurry del ejercicio anterior:
   let uncurry f (a, b) = f a b;;  y le pasamos la función
   comp, podemos observar que nos devuelve la composición
   de funciones de forma uncurry:
        let ucomp = uncurry comp;;
        ucomp (square, ((+) 1)) 3;; = 16
 *)