(* Título:  Práctica 7 - Merge Sort (parte 2) *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

let rec divide l =
  match l with
    h1::h2::t ->
      let t1, t2 = divide t
    in (h1::t1, h2::t2)
  | _ -> l, [];;

(*Función redefinida con tipo ('a -> 'a -> bool) *)
let rec merge ord (l1, l2) =
  match (l1, l2) with
    ([], l) | (l, []) -> l
  | (h1::t1, h2::t2) ->
    if ord h1 h2
      then h1::(merge ord (t1, h2::t2))
    else h2::(merge ord (h1::t1, t2));;

(*Función redefinida con tipo ('a -> 'a -> bool) *)
let rec msort1 ord l =
  match l with
    [] | _::[] -> l
  | _ ->
    let l1, l2 = divide l
  in merge ord (msort1 ord l1, msort1 ord l2);;

(*
 *    La no terminalidad si puede provocar problemas cuando el vector a ordenar
 * es muy grande (del orden de 300000 ya da stack overflow). Ejemplo de esto es la 
 * lista l2:
 *)

let l2 = List.init 300000 abs;;

(* Funciones recursivas terminales *)

let divide' l = 
  let rec aux (r1, r2) l =
    match l with
      h1::h2::t -> aux (h1::r1, h2::r2) t
    | h::[] -> aux (h::r1, r2) []
    | [] -> (List.rev r1, List.rev r2)
  in aux ([],[]) l;;

let merge' ord (l1, l2) =
  let rec aux l (l1, l2) =
    match (l1, l2) with
    | ([], []) -> List.rev l
    | ([], h::t) -> aux (h::l) ([], t)
    | (h::t, []) -> aux (h::l) (t, [])
    | (h1::t1, h2::t2) ->
      if ord h1 h2
        then aux (h1::l) (t1, h2::t2)
      else aux (h2::l) (h1::t1, t2)
  in aux [] (l1, l2);;

let rec msort2 ord l =
  match l with
    [] | _::[] -> l
  | _ ->
    let l1, l2 = divide' l
  in merge' ord (msort2 ord l1, msort2 ord l2);;

(* Comparación de tiempos: *)

let crono f x =
  let t = Sys.time() in
  let _ = f x in
  Sys.time() -. t;;

let rec qsort2 ord =
  let append' l1 l2 = List.rev_append (List.rev l1) l2 in
  function
    [] -> []
  | h::t -> let af, bf = List.partition (ord h) t in
            append' (qsort2 ord bf) (h :: qsort2 ord af);;

(*
 *     Con la ejecución de: crono (X(B)) L, podremos saber cuanto tarda cada algoritmo
 * X en ordenar una lista L en orden B. Resultados:
 *     - Lista pequeña ordenada (2000 elementos del 0 al 4999):
 *          -> qsort2: 0.205288 segundos.
 *          -> msort1: 0.003216 segundos.
 *          -> msort2: 0.004626 segundos.   
 *     - Lista pequeña desordenada (2000 elementos aleatorios):
 *          -> qsort2: 0.004288 segundos.
 *          -> msort1: 0.006678 segundos.
 *          -> msort2: 0.009661 segundos.  
 *     - Lista grande ordenada (200000 elementos del 0 al 199999):
 *          -> qsort2: FALTA DE MEMORIA -> Imposible ejecutar.
 *          -> msort1: STACK OVERFLOW.
 *          -> msort2: 0.629047 segundos.  
 *     - Lista grande desordenada (200000 elementos aleatorios):
 *          -> qsort2: 0.549182 segundos.
 *          -> msort1: STACK OVERFLOW.
 *          -> msort2: 0.628581 segundos.  
 *    Lo primero que detectamos al analizar los tiempos es que quicksort para listas ordenadas es
 * extremadamente lento (llegando a ser 60 veces mejor el merge sort). La diferencia en este caso entre
 * ambos algoritmos de fusión es de un 30% (siendo msort1 1.3 veces más rápido).
 *    Para el caso de un vector pequeño y desordenado (elementos aleatorios del rango [0, ..., N-1])
 * observamos que msort2 es el más lento, con una diferencia del 60% y del 30% con qsort2 y msort1
 * respectivamente (siendo qsort2 2.2 veces más rápido y msort1 un 1.6).
 *    Finalmente, en el caso de las listas grandes vemos como msort1 da siempre stack overflow debido
 * a la no terminalidad de las funciones merge y divide. En qsort2 vemos como es muy rápido en el
 * vector desordenado, pero no acaba si agotar memoria cuando este está ordenado. En el caso del aleatorio
 * qsort2 es 1.2 veces más rápido que msort2.
 *    Como conclusiones podemos decir que para vectores pequeños msort1 es de media el más eficaz, ya que
 * qsort para ordenados es muy muy lento. Para vectores grandes no nos queda otra que utilizar msort2.
 *)

(*    Importante mencionar que la medición de tiempos dependerá de las características de la máquina,
 * del estado de la misma y de factores externos. A mayores, los tiempos no incluyen el proceso de
 * generar los vectores. Las características básicas de la máquina (HP Omen 15-ax014ns) son:
 *    - Intel Core i5-6300HQ
 *    - 8GB RAM DDR4 a 2666MHz
 *)