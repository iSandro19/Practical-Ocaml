(* Título:  Práctica 3 - Ejercicios 1, 2 y 3  *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

(* PRIMERA PARTE *)

(* Función hd *)
let hd l =
  match l with
    t::_ -> t
  | [] -> raise (Failure "hd");;

(* Función tl *)
let tl l =
  match l with
    _::t -> t
  | [] -> raise (Failure "tl");;

(* Función length *)
let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t;;

(* Función compare_lengths *)
let rec compare_lengths l1 l2 =
  match (l1, l2) with
    [],[] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | _::t1, _::t2 -> compare_lengths t1 t2;; 
    
(* Función nth *)
let rec nth l p =
  match l with
    [] -> raise (Failure "nth")
  | h::t -> if p = 0 then h
            else nth t (p-1);;  

(* Función append *)
let rec append l1 l2 =
  match l1 with
    [] -> l2
  | h::t -> h::append t l2;;


(* SEGUNDA PARTE *)

(* Función find *)
let rec find c l=
  match l with
    [] -> raise (Failure "Not found")
  | h::t -> if (c h) then h
            else find c t;;

(* Función for_all *)
let rec for_all c l =
  match l with
    [] -> true
  | h::t -> if c h then for_all c t
            else false;;

(* Función exists *)
let rec exists c l =
  match l with
    [] -> false
  | h::t -> if c h then true
            else exists c t;;

(* Función mem *)
let rec mem i l =
  match l with
    [] -> false
  | h::t -> i = h || mem i t;;

(* Función filter *)
let rec filter c l =
  match l with
    [] -> []
  | h::t -> if c h then h::filter c t 
            else filter c t;;

(* Función find_all *)
let rec find_all c l =
  match l with
    [] -> []
  | h::t -> if c h then h::find_all c t 
            else find_all c t;;

(* Función partition *)
let rec partition c l =
  match l with
    [] -> ([],[])
  | h::t -> if c h then (h::fst(partition c t), snd(partition c t))  
            else (fst(partition c t), h::snd(partition c t));;

(* Función split *)
let rec split l =
  match l with
    [] -> ([],[])
  | h::t -> (fst h::fst(split t), snd h::snd(split t));;

(* Función combine *)
let rec combine l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (h1::t1, h2::t2) -> (h1,h2)::(combine t1 t2)
  | ([], _::_) | (_::_, []) -> raise (Failure "Invalid_Argument");;

(* TERCERA PARTE *)
let rec init len f =
  match (len >= 0) with
    true -> if len = 0 then []
            else (f len)::init (len-1) f
  | false -> raise (Failure "Invalid_argument");;