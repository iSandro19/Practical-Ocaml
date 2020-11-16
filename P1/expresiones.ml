(* Título:  Práctica 1 - expresiones          *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

();;
(* - : unit = () *)

2 + 5 * 3;;

(* - : int = 17 *)

1.0;;

(* - : float 1.0 *)

(* 1.0 * 2;; 
 Error de tipo debido a mezcla de un float y un int.
 *)

(* 2 - 2.0;; 
 Error de tipo debido a mezcla de un int y un float.
 *)

(*3.0 + 2.0;;
 Error de tipo, ya que el operador suma es el de enteros
 *)

5 / 3;;
(* - : int = 1 *)

5 mod 3;;
(* - : int = 2 *)

3.0 *. 2.0 ** 3.0;;
(* - : float = 24  ---> Porque ** es el operador de la exponenciación y
tiene prioridad.
 *)

3.0 = float_of_int 3;;
(* - : bool = true *)

(* sqrt 4;; 
 Error de tipo debido al uso de un int en una función float.
 *)

int_of_float 2.1 + int_of_float (-2.9);;
(* - : int = 0 *)

truncate 2.1 + truncate (-2.9);;
(* - : int = 0 *)

floor 2.1 +. floor (-2.9);;
(* - : float = -1 ---> Importante! Devuelve un float*)

ceil 2.1 +. ceil (-2.9);;
(* - : float = 1 ---> Importante! Devuelve un float*)

2.0 ** 3.0 ** 2.0;;
(* - : int = 512 ---> No da 64, ya que se anidan los exponentes,
 lo que nos daría 2^(3^2) = 512
 *)

'B';;
(* - : char = 'B' *)

int_of_char 'A';;
(* - : int = 65 *)

char_of_int 66;;
(* - : char = 'B' *)

Char.code 'B';;
(* - : int = 66 *)

Char.chr 67;;
(* - : char = 'C' *)

'\067';;
(* - : char = 'C' *)

Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'M');;
(* - : char = 'm' ---> ya que nos da 109 que en asci es la "m"*)

Char.uppercase 'm';;
(* - : char = 'M' ---> Importante! Nos da un aviso de que está en desuso*)

Char.lowercase 'O';;
(* - : char = 'o' ---> Importante! Nos da un aviso de que está en desuso*)

"this is a string";;
(* - : string = "this is a string" *)

String.length "longitud";;
(* - : int = 8 *)

(* "1999" + "1";;
 Error de tipo debido al uso de string en un operador int. 
 *)

"1999" ^ "1";;
(* - : string = "19991" *)

int_of_string "1999" + 1;;
(* - : int = 2000 *)

"\064\065";;
(* - : string = "@A" ---> El '\064' es el '@' *)

string_of_int 010;;
(* - : string = "10" ---> Importante! Los 0 del inicio se omite*)

not true;;
(* - : bool = false *)

true && false;;
(* - : bool = false ---> Importante! Saber que se evalúa primero la izq *)

true || false;;
(* - : bool = true ---> Importante! Saber que se evalúa primero la izq *)

(1 < 2) = false;;
(* - : bool = false ---> Importante! Saber que se evalúa primero la izq *)

"1" < "2";;
(* - : bool = true ---> Importante! Los strings van en orden alfanumérico *)

2 < 12;;
(* - : bool = true *)

"2" < "12";;
(* - : bool = false ---> Importante! Los strings van en orden alfanumérico 
 y se evalúan los dígitos de forma ordenada => 1000 < 2
 *)

"uno" < "dos";;
(* - : bool = false ---> Importante! Los strings van en orden alfanumérico *)

if 3 = 4 then 0 else 4;;
(* - : int = 4 *)

if 3 = 4 then "0" else "4";;
(* - : string = "4" *)


(* if 3 = 4 then 0 else "4";;
 Error de tipo debido a que la función al ver el 0 espera tener los valores
 de retorno del mismo tipo => No se pueden mezclar tipos!
 *)

(if 3 < 5 then 8 else 10) + 4;;
(* - : int = 12 *)

2.0 *. asin 1.0;;
(* - : float = 3.1415... ---> Cálculo de pi *)

sin (2.0 *. asin 1.0 /. 2.);;
(* - : float = 1. ---> Es el seno de 90º (pi/2) *)

function x -> 2 * x;;
(* - : int -> int = <fun> *)

(function x -> 2 * x) (2 + 1);;
(* - : int = 6 ---> Definimos la función y metemos sus entradas a la drch *)

let x = 1;;
(* val x : int = 1 *)

let y = 2;;
(* val y : int = 2 *)

x - y;;
(* - : int = -1 *)

let x = y in x - y;;
(* - : int = 0 ---> Importante! La palabra reservada in solo modifica
 el valor para esta operación. x = 2; 2 - 2 = 0;
 *)

x - y;;
(* - : int = -1 ---> No cambia el valor como se ha mencionado *)

(* z;;
 Error léxico debido a que no existe un valor en z.
 *)

let z = x + y;;
(* val z : int = 3 *)

z;;
(* - : int = 3 *)

let x = 5;;
(* val x : int = 5 *)

z;;
(* - : int = 3 *)

let y = 5 in x + y;;
(* - : int = 10 *)

x + y;;
(* - : int = 7 ---> Vuelve a no cambiarse el valor de y *)

let x = x + y in let y = x * y in x + y + z;;
(* - : int = 24 ---> 7 + 14 + 3 *)

x + y + z;;
(* - : int = 10 ---> Vuelven a no cambiarse los valores *)

int_of_float;;
(* - : float -> int = <fun> *)

float_of_int;;
(* - : int -> float = <fun> *)

int_of_char;;
(* - : char -> int = <fun> *)

char_of_int;;
(* - : int -> char = <fun> *)

abs;;
(* - : int -> int = <fun> *)

sqrt;;
(* - : float -> float = <fun> *)

truncate;;
(* - : flaot -> int = <fun> *)

ceil;;
(* - : float -> float = <fun> *)

floor;;
(* - : float -> float = <fun> *)

Char.code;;
(* - : char -> int = <fun> *)

Char.chr;;
(* - : int -> char = <fun> *)

Char.uppercase;;
(* - : char -> char = <fun> *)

Char.lowercase;;
(* - : char -> char = <fun> *)

int_of_string;;
(* - : string -> int = <fun> *)

string_of_int;;
(* - : int -> string = <fun> *)

String.length;;
(* - : string -> int = <fun> ---> Supongo que se deben poner los dos ";" *)

let f = function x -> 2 * x;;
(* val f : int -> int = <fun> *)

f (2 + 1);;
(* - : int = 6 ---> El doble de 3 *)

f 2 + 1;;
(* - : int = 5 ---> El (doble de 2) + 1 *)

let n = 1;;
(* val n : int = 1 *)

let g x = x + n;;
(* val g : int -> int = <fun> *)

g 3;;
(* - : int = 4 *)

let n = 5;;
(* val n : int = 5 *)

g 3;;
(* - : int = 4 ---> Importante! No da 8 porque no se ha vuelto
 a definir que era g después de cambiar el valor de n
 *)