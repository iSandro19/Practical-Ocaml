(*
 * En OcamL las expresiones se evalúan de la siguiente forma:
 *  - Comparaciones lógicas: exp1 && exp2 --> evalúa solo exp1
 *  - Funciones: función argumentos --> evalúa primero los argumentos
 *
 * Ojo!
 *  - Diferencias C vs OcamL:
 *    # == vs =
 *    # != vs <>
 *    # ! vs not
 *)

print_endline "Hello World!"

let x = 10;;
let y = 5;;

a + b;;

let x = 1024 in x*x*x;;

let xy = let x = 1024 in x*x*x;;

let allbooltrue = function true -> true | false -> true;;
let allbooltrue = function (_:bool) -> true;;
let allbooltrue : bool -> bool = function _ -> true;;

let alltrue = function _ -> true;;

allbooltrue('a' = 'A');;

let notzero = function 0 -> false | _ -> true;;

(*CUIDADO CON EL ORDENx - y;; DE LAS ESPECIFICACIÓN DE LAS FUNCIONES*)
let becareful = function _ -> true | 0 -> false;;

(* No hace falta especificar el tipo, caml lo deduce *)
let doble = function int -> int*2;;
let doblx = function x -> x*2;;

let fdoble = function float -> float*.2.0;;
let fdoblx = function x -> x*.2.0;;

let f = function int -> int*2;;

let f = function x -> function y -> x + y;;
let f x = function y -> x + y;;
let f x y = x + y;;
(* val f : int -> int -> int = <fun> *)
(* Ojo! se lee (int -> (int -> int)) *)
(* Son las 3 formas de escribir lo mismo *)

let add = (+) 1;;
add 5;; (* - : int = 6 *)

let f (x, y) = x + y;;

snd;; (* Second *)

fst;; (* First *)

fst(2, 3);;
snd(2, 3);;

let rec fact n =
		if n = 0 then 1
		else n*fact(n-1);;

fact 6;;  (* - : int = 720 *)
fact 20;; (* - : int = 2432902008176640000 *)
fact 30;; (* - : int = 458793068007522304  => Overflow *)
max_int;; (* - : int = 4611686018427387903 *)
fact 64;; (* - : int = 0 *)
fact 300000;; (* Stack overflow during evaluation (looping recursion?). *)

let rec fib n =
		if n <= 1 then n
		else fib (n - 1) + fib (n - 2);;

let f x = x + 1, x - 1;;

let f (x, y) = x + y, x*y;;
let f x y = x + y, x*y;;      (* Función curry equivalente a la anterior *)

(* Funciones recursivas para calcular el cociente y el resto por separado *)

let rec cociente dividendo divisor =
		if dividendo < divisor then 0
		else 1 + cociente (dividendo - divisor) divisor;;

let rec resto dividendo divisor =
		if dividendo < divisor then dividendo
		else resto (dividendo - divisor) divisor;;

let div x y = cociente x y, resto x y;; (* Par cociente resto *)

(* Podemos aumentar el rendimiento metiendo ambos en una función recursiva *)

let rec div dividendo divisor =
		if dividendo < divisor then 0, dividendo
		else let q, r = div (dividendo - divisor) divisor
				 in 1 + q, r;;

let crono f x =
		let t = Sys.time() in
		let _ = f x in
		Sys.time() -. t;;

let tiempo_fib = (1. +. sqrt 5.) /. 2.;;

(* Para obtener 2 o más resultados de una misma función necesitaremos el producto cartesiano *)
let f x = x+1, x-1;;
let g x y = x+y, x*y, x-y, x/y;;

let per_area r =
		let pi = 2. *. asin 1. in       (* Al calcular pi de forma local evitamos calcular pi cada vez que lo necesite *)
				2. *. pi *. r,              (* Calculamos el perímetro *)
				pi *. (r *. r);;            (* Calculamos el área *)

(* El problema es que pi se calcula cada vez que llamamos a per_area, una mejor forma sería: *)

let per_area =                      (* De esta forma evitamos que se repita el cálculo de pi *)
		let pi = 2. *. asin 1. in
				function r -> 2. *. pi *. r,
											pi *. (r *. r);;

let rec quo x y =
		if x < y then 0
		else 1 + quo (x-y) y;;

let rec fib2 = function
		1 -> 1, 0
	| n -> let f1, f2 = fib2 (n-1) in
				 f1 + f2, f1;;

let fib n =
		fst (fib2 n);;

(* Listas *)

[1; 2; 3; 10];; (* int list = [1; 2; 3; 10] *)
[1+3; 5*5; fib 10];;
[];;    (* 'a list = [] *)
[(); ()];;  (* unit list = [(), ()] *)
['a'; 'e'; 'i'; 'o'; 'u'];; (* char list = ['a'; 'e'; 'i'; 'o'; 'u'] *)

List.length;;
List.length [];; (* int = 0 *)
List.length [1+3; 5*5; fib 10];; (* int = 2 *)

List.hd [1; 2; 3];; (* Header;; int = 1 *)
List.tl [1; 2; 3];; (* Tail;; int list = [2; 3] *)

(@);; (* 'a list -> 'a list -> 'a list = <fun> *)
[1; 2; 3] @ [4; 5; 6];; (*Concatenación;; int list = [1; 2; 3; 4; 5; 6] *)

List.append;; (* Es igual al operador infijo  @ *)
List.append [1][2];;

List.concat;; (* Es la generalización de append, concatena las n listas *)
List.concat [[1]; [2]];;

List.rev [3; 2; 1];; (* Reverse;; int list = [1; 2; 3] *)

List.nth [1; 2; 3] 0;; (* Devuelve el valor de la posición n;; int = 0 *)

List.map float_of_int [-1; 5; 10];; (* Aplicación de una función en una lista *)
List.map (function n -> 2 * n) [1; 3; 5; 7];;

List.filter (function n -> n > 0) [1; -3; 2; 0; 7; -89];;
List.filter ((<) 0) [1; -3; 2; 0; 7; -89];;

List.mem 2 [1; -3; 2; 0; 7; -89];; (* bool = true *)
List.mem 2 [1; -3; 0; 7; -89];; (* bool = false *)

List.exists ((<) 0) [1; 2; -3];; (* Si hay alguno que cumpla la condición *)

List.for_all ((<) 0) [1; 2; -3];; (* Si todos cumplen la condición *)

List.find ((>) 0) [1; 2; -3];; (* Devuelve el primer elemento que cumpla la condición *)

List.init;; (* En mi versión no está definida (4.05.0) *)

2::3::4::[];; (* Listas en ocaml en realidad *)

let hd = function h::_ -> h;;

let hd (h::_) = h;;
let tl (_::t) = t;;

let tl = function
		_::t -> t
		| [] -> raise (Failure "tl");;

let rec last = function
		h::[] -> h
		| h::t -> last t;;

let rec sorted = function
		h1::h2::t -> h1 <= h2 && sorted (h2::t)
| _ -> true;; (* No recursiva terminal! *)

let insert_sort l =
		let rec aux ord = function
				[] -> ord
		| h::t -> aux (insert' h ord) t
in aux [] l;;

(* Ordenación*)

let rec fusion l1 l2 =
		match l1, l2 with
						[], l | l, [] -> l
				| h1::t1, h2::t2 ->
						if h1 <= h2
								then h1::fusion t1 l2
						else h2.:fusion l1 t2;;

let rec divide = function
		h1::h2::t ->
			let t1, t2 = divide t
		in h1::t1, h2::t2
	| l -> l, [];;

let rec merge l =
	match l with
			[] | [_] -> l
		| _ -> 
			let l1, l2 = divide l in
	fusion (merge l1) (merge l2);;

(* Problema de las reinas *)

let in_danger (i1,j1) (i2,j2) =
		i1 = i2 ||
		j1 = j2 ||
		abs(i2-i1) = abs(j2-j1);;

let rec compatible p = function
				[] -> true
		| h::t -> not (in_danger p h) && compatible p t;;

let reinas n = (* n >= 0 *)
		let rec completa path (i,j) =
				if i > n then [path]
				else if j > n then []
				else if compatible (i,j) path
						then match completa ((i,j)::path) (i+1,1) with
								[] -> completa path (i, j+1)
							 | s -> s
				else completa path (i,j+1)
		in completa [] (1,1);;