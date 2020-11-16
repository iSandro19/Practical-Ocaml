(* Título:  Práctica 1 - expresiones2         *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

let u = (4 + 8) / 9 * 1 - 3 mod 1;;

let v = sqrt (float_of_int 9);;

let w = char_of_int (let v = 9 in v*v);;

let x = (10 < int_of_float 5.) && ((function x -> x*2) 10 > abs (int_of_char 'A'));;

let y = if ((function x -> x*x) 10 = int_of_char 'X')
                then "El valor de 10^2 es IGUAL al valor entero de X"
        else "El valor de 10^2 es DIFERENTE al valor entero de X";;