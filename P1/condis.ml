(* Título:  Práctica 1 - condis               *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

false && (2 / 0 > 0);;
(* - : bool = false *)

(* true && (2 / 0 > 0);;
   Error: Se divide por cero
 *)

true || (2 / 0 > 0);;
(* - : bool = true *)

(* false || (2 / 0 > 0);;
   Error: Se divide por cero
 *)

let con = (&&);;
(* val con : bool -> bool -> bool = <fun> *)

let dis = (||);;
(* val dis : bool -> bool -> bool = <fun> *)

(&&) (1 < 0) (2 / 0 > 0);;
(* - : bool = false ---> Importante! En este caso solo
   se evalúa el primer caso.
 *)

(* con (1 < 0) (2 / 0 > 0);;
   Error: Se divide por cero ---> IMPORTANTE! Al definir
   nosotros la función, evalúa ambos argumentos.
 *)

(||) (1 > 0) (2 / 0 > 0);;
(* - : bool = true ---> Importante! En este caso solo
   se evalúa el primer caso.
 *)

(* dis (1 > 0) (2 / 0 > 0);;
   Error: Se divide por cero ---> IMPORTANTE! Al definir
   nosotros la función, evalúa ambos argumentos.
 *)