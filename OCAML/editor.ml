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

(* Árboles *)

type 'a tree =
		V
	| N of 'a * 'a tree * 'a tree;;

let t = Node (3, Node (8, Empty, Empty),
								 Node (2, Node (5, Empty, Empty),
													Node (1, Empty, Empty)));;

let rec nnodos = function
		V -> 0
	| N (_,i,d) -> 1 + nnodos i + nnodos d;;

let rec altura = function
		V -> 0
	| N (_,i,d) -> 1 + (max (altura i) (altura d));;
													
let rec sum = function
		Empty -> 0
	| Node (x, l, r) -> x + (sum 1) + (sum r);;

let rec prod = function
		Empty -> 1.0
	| Node (x, l, r) -> x *. (prod 1) *. (prod r);;

let rec mirror = function
		Empty ->  Empty
	| Node (x, l, r) -> Node (x, mirror r, mirror l);;

type 'a option =
		Some of 'a
	| None;;

type numero =
	F of float | I of int;;

let suma n1, n2 =
	match n1, n2 with
			I x, I y -> I (x+y)
		| F x, F y -> F (x+.y)
		| I x, F y -> F ((float x) + .y)
		| F x, I y -> F (x +. (float y));;

type 'a btree =
		Leaf of 'a
	| Node of 'a * 'a btree * 'a btree;;

let l x = Leaf x;;

let b1 = Node (6, l 5, l 11);;

type 'a gtree =
	GT of 'a * 'a gtree list;;

let h x = GT (x, []);;

let g = GT (2, [GT (7, [h 2; h 10; GT (6, [h 5; h 11])]); GT (5, [GT (9, [h 4])])]);;

let rec chained = function
  [] | [_] -> true
  | (x1,y1)::((x2,y2)::t as l) ->
      let dx = abs (x1-x2) and dy = abs (y1-y2) in
          ( dx = 1 && dy = 2 || dx = 2 && dy = 1 ) &&
          not (List.mem (x1,y1) l) &&
					chained l;; 
					
(* Entrada y salida *)

output_char;;
stdout;;
output_char stdout 'X';;

let print_char c = output_char stdout c;; (* Ya definida *)

let _ = print_char 'A' in let _ = print_char 'B' in let _ = print_char 'C' in print_char '\n';;
print_char 'A' ; print_char 'B' ; print_char 'C' ; print_char '\n';; (* Composición secuencial *)

let output_string canal s =
	let n = String.length s in
		let rec loop i =
			if i >= n then ()
			else (output_char canal s.[i];
						loop (i+1))
		in loop 0;;

let print_string s = output_string stdout s;;
let print_endline s = output_string stdout (s ^ '\n');;
let print_newline () = print_endline "";;

input_char;;
stdin;;
input_char stdin;;

input_line;;

open_out;;
let canal = open_out "prueba";;

flush canal;;

close_out canal;;

let rec output_string_list out = function
		[] -> ()
	| h::t -> output_string out (h ^ "\n");
						output_string_list out t;;

let rec iter p = function
		[] -> ()
	| h::t -> p h ; iter p t;;

let output_string_list out l =
	List.iter (fun s -> output_string out (s ^ "\n")) l;;

let rec input_string_list inp =
	try let s = input_line inp in
		s :: input_string_list inp
	with End_of_file -> [];;

pos_in;; (* Current position reading at in_channel *)

(* Imperativo *)

let i = ref 8;; (* ref content = 8 *)
(!) i;; (* int = 8 *)
!i

(* Bucles for *)
for i = 1 to 9+1 do print_string (string_of_int i) done;;

let fact n = 
	let f = ref 1 in 
	for i = 1 to n do
		f := !f * i
	done;
	!f;;

(* Bucles while *)
let fact n =
	let f = ref 1
	and i = ref 1 in
	while !i <= n do
		f := !f * !i;
		i := !i + 1
	done;
	!f;;

(* Variables globales *)
let n = ref 0;;

let turno () =
	n := !n + 1;
	!n;;

let reset () =
	n := 0;;

(* Variables locales *)
let turno =
	let n = ref 0 in
	fun () ->
		n := !n + 1;
		!n;;

(* Módulos en el propio código *)
module Counter : sig
	val turno : unit -> int
	val reset : unit -> unit
end =
struct
	let n = ref 0
	let turno () =
		n := !n + 1;
		!n
	let reset () =
		n := 0
end

(* Functor -> Nos permite hacer diferentes modulos *)
module Counter () : sig
	val turno : unit -> int
	val reset : unit -> unit
end =
struct
	let n = ref 0
	let turno () =
		n := !n + 1;
		!n
	let reset () =
		n := 0
end

(* Arrays *)
[||];;
[|1;2;3|];;
let v = [|1;2;3|];;

v.(0);;
Array.length v;;
Array.get v 1;;
Array.set v 1 100;;
v.(1) <- 30;;

Array.make 10 0;;
Array.init 10 (fun x -> x);;

Array.sort compare v;;
v;;

let w = Array.copy v;;

let vprod v1 v2 =
	if Array.length v1 = Array.length v2
	then
		let p = ref 0. in
		for i = 0 to Array.length v1 - 1 do
			p := !p +. v1.(i) *. v2.(i)
		done;
		!p
	else raise (Invalid_argument "vprod");;

let vprod v1 v2 =
	Array.fold_left (+.) 0. (Array.map2 ( *.) v1 v2);;

(* Registros (structs) *)
type persona = {nombre : string; edad : int};;

let p1 = {nombre "Sandro"; edad = 19};;
p1.nombre;; p1.edad;;
let mas_viejo p = {p with edad = p.edad + 1};;

type persona = {nombre : string; mutable edad : int};;
let p2 = {nombre "Maria"; edad = 19};;
p2.edad <- 18;;

let envejece p = p.edad <- p.edad + 1;;

(* POO *)

(* Objetos *)
let c1 = object
	val mutable n = 0
	method next = n <- n + 1; n
	method reset = n <- 0
end;;

c1#next;;

let double_next c = 2 * c#next;;

let double c = object
	method next = 2 * c#next;
	method reset = c#reset
end;;

let c2 = double c1;;

(* Clases *)
class counter = object
	val mutable n = 0
	method next = n <- n + 1; n
	method reset = n <- 0
end;;

let c3 = new counter;;

c1#reset;;
[c1#next;c1#next;c1#next];; (*[3;2;1]*)

class counter_set = object
	val mutable n = 0
	inherit counter (* Herencia *)
	method set ini = n <- ini
end;;

let cc = new counter_set;;

class counter_init n0= object (self)
	val mutable n = 0
	inherit counter_set
	initializer self#set n0 (* Constructor *)
end;;

