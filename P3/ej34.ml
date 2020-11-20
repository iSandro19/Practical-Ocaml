(* Título:  Práctica 3 - Ejercicios 1, 2 y 3  *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

(* Ejercicio 4: Goldbach *)
let goldbach n =
  if (n mod 2) <> 0
    then raise (Invalid_argument "goldbach")
  else
    let rec aux d =
      if
        let rec prm d =
          d * d > n || (n mod d <> 0 && prm (d+1)) in
        prm d && prm (n-d) 
        then (d, n-d)
      else aux (d+1)
    in aux  2;;