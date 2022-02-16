(* Título:  Práctica 8 - Problema del caballo *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

(* Función para comprobar si (x,y) se encuentra dentro de los límites del tablero *)
let isInsideBoard n m (x,y) =
  x >= 1 &&
  x <= n &&
  y >= 1 &&
  y <= m ;;

(* Función para comprobar si el elemento "e" se encuentra en la lista l *)
let elementInList l e =
  not (List.mem e l);;

(* Función para realizar los movimientos *)
let movements (x,y) n m visited =
  let permittedMoves = [(x+2, y-1); (x+1, y-2); (x-1, y-2); (x-2, y-1); 
                        (x-2, y+1); (x-1, y+2); (x+1, y+2); (x+2, y+1)]
  (* Filtramos los movimientos posibles (los que no se hayan hecho o no se salgan del tablero) *)
  in List.filter (elementInList visited) (List.filter (isInsideBoard n m) permittedMoves);;

let tour n m i f =
  (* Primero miramos que los valores se encuentre dentro del tablero *)
  if not (n >= 1 && m >= 1 && isInsideBoard n m i && isInsideBoard n m f)
    then raise (Invalid_argument "tour")
  else
    let rec aux path = function
      [] -> raise Not_found (* No se ha encontrado solución *)
    | h::t ->
      if (h = f) (* Encontramos el final *)
        then List.rev (h::path)
      else
        try aux (h::path) (movements h n m (h::path)) with
          Not_found -> aux path t
  in
    if i = f (* Si init y final son iguales, devolvemos esa posición *)
      then [i]
    else aux [i] (movements i n m [i]);; (* En caso contrario, buscamos la solución *)