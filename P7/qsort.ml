(* Título:  Práctica 7 - Quicksort (parte 1)  *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

(* Funciones: *)

let rec qsort1 ord = function
    [] -> []
  | h::t -> let af, bf = List.partition (ord h) t in
            qsort1 ord bf @ h :: qsort1 ord af;;

let rec qsort2 ord =
  let append' l1 l2 = List.rev_append (List.rev l1) l2 in
  function
    [] -> []
  | h::t -> let af, bf = List.partition (ord h) t in
            append' (qsort2 ord bf) (h :: qsort2 ord af);;

(*
 *    Esta implementación tendrá un nefasto rendimiento si el vector que queremos
 * está ya ordenado (tanto ascendentemento como descendentemente) o deja muchos elementos
 * a un lado. Esto se debe a que tiene que encontrar el primer valor que cumple la 
 * condición y, si el vector está ordenado puede darse el caso que tenga que recorrerlo
 * todo para encontrar dicho elemento dejando todos los elementos a un lado. De esta forma,
 * siempre que se dejen muchos elementos a un lado necesitaremos una enorme cantidad de
 * memoria para ejecutar el cálculo.
 * 
 *    Ejemplo de esto:
 *      - Array = [0, 1, 2, 3, ..., 299999]
 *      - Cogemos el 0 y buscamos el primer elemento menor
 *      - Como no hay nminguno hai que recorrer todo el array
 *      - Asi sucesivamente...
 *   
 *    Esto no es el único problema. Quicksort 1 no es recursivo terminal, por lo que
 * no podrá ordenar arrays de gran tamaño como el que se define a continuación:
 *)

let l1 = List.init 1000000 (fun x -> (Random.int 999999));;

(* 
 *    Esta lista l1 no puede ordenarse con qsort1, pero si con qsort2, por culpa de la
 * recursividad terminal.
 *    Pasemos a analizar el tiempo de ejecución:
 *)

let crono f x =
  let t = Sys.time() in
  let _ = f x in
  Sys.time() -. t;;

(*
 *     Con la ejecución de: crono (qsortX(B)) l, podremos saber cuanto tarda cada algoritmo
 * X en ordenar una lista l en orden B. Resultados:
 *     - Lista pequeña ordenada (5000 elementos del 0 al 4999):
 *          -> qsort1: 1.253791 segundos.
 *          -> qsort2: 1.348843 segundos.       
 *     - Lista pequeña desordenada (5000 elementos aleatorios del 0 al 4999):
 *          -> qsort1: 0.018216 segundos.
 *          -> qsort2: 0.024850 segundos.
 *     - Lista grande ordenada (10000 elementos del 0 al 9999):
 *          -> qsort1: 5.753855 segundos (para vectores más grandes agota memoria).
 *          -> qsort2: 5.362176 segundos (para vectores más grandes agota memoria).
 *     - Lista grande desordenada (200000 elementos aleatorios del 0 al 199999):
 *          -> qsort1: 0.508179 segundos.
 *          -> qsort2: 0.553708 segundos.
 *      Haciendo una media de los resultados, tenemos que qsort2 tarda entre un 10% y un 7% más
 *  de media, con algún caso dispar donde salta al 20%. Cabe destacar que es recursiva terminal
 *  a cambio.
 *)

(*    Importante mencionar que la medición de tiempos dependerá de las características de la máquina y
 * del estado de la misma. A mayores, los tiempos no incluyen el proceso de generar los vectores.
 * Las características básicas de la máquina (HP Omen 15-ax014ns) son:
 *    - Intel Core i5-6300HQ
 *    - 8GB RAM DDR4 a 2666MHz
 *)