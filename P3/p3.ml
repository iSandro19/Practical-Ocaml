(* Título:  Práctica 3 - Ejercicios 1, 2 y 3  *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

(* Ejercicio 1: Greatest Commond Divisor *)
let rec gcd a b = 
  if b = 0            (* Si b es igual a 0: *)
    then a            (* Devolvemos a. *)
  else                (* En caso contrario: *)
    gcd b (a mod b);; (* Hacemos el MCD de b y del módulo de a y b. *)

(* Ejercicio 2: Explicación de función is_prm *)
let is_prm n =
  let rec not_divisible_from d =
    d * d > n || (n mod d <> 0 && not_divisible_from (d+1)) in
  n > 1 && not_divisible_from 2;;

(*    Esta función nos devuelve cierto si el entero pasado por parámetro "n"
  es un número primo o falso en caso contrario. De este modo, el tipo
  de la función será  int -> bool = <fun>.

      Para calcular dicho resultado, lo que hace es crear una función local
  llamada not_divisible_from de parámetro "d" que, de forma recursiva, mira que:
     - d^2 sea mayor que n, o que:
     - el resto de n/d sea diferente de 0.
      De esta forma, si alguna de estas condiciones es verdadera (si es la primera
    ya no se evalúa la segunda) devuelve true. De lo contrario devuelve false.
*)

let is_prm2 n =
  let rec not_divisible_from d =
    (n mod d <> 0 && not_divisible_from (d+1)) || d * d > n in
  n > 1 && not_divisible_from 2;;

(*    Esta función hace lo mismo, con la diferencia de orden en las condiciones.
  En la disyunción, si el primer operando es verdadero no se evalúa el segundo, lo
  que implica que siempre pasemos por la condición que tiene la recursividad.
*)

(*    Las diferencias entre ambas se ven claramente al calcular el valor de 999983
  para cada una. En la primera nos proporciona el valor bool = true, mientras que en
  la segunda nos da stack overflow. Esto se debe al orden de condiciones en la
  función not_divisible_from. La primera condición se evalúa siempre, por lo que si
  situamos ahí la recursividad puede que acabemos la pila de recursividad.
*)


(* Ejercicio 3: capicúa *)
let capicua n =
  let rec reverse n1 n2 =
    if n1 <> 0    (* Mientras n1 sea diferente de 0 *)
      then (reverse (n1/10) ((n1 mod 10) + n2*10))  (* Calculamos el inverso de n *)
    else
      n2  (* Al acabar devolvemos el número invertido *)  
  in (n = reverse n 0);;  (* Si n es igual a su inverso, true; en caso contrario false *)