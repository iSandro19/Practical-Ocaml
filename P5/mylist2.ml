(* Título:  Práctica 5 - Ejercicios listas    *)
(* Author:  Óscar Alejandro Manteiga Seoane   *)
(* Login:   oscar.manteiga@udc.es             *)
(* Grupo:   4.5                               *)

(* PRIMERA PARTE *)

(* Función hd *)
let hd l =
  match l with
    h::_ -> h
  | [] -> raise (Failure "hd");;

(* Función tl *)
let tl l =
  match l with
    _::t -> t
  | [] -> raise (Failure "tl");;

(* Función length *)
let rec length l =
  let rec aux len = function
    [] -> len
  | _::t -> aux (len+1) t in
  aux 0 l;;

(* Función compare_lengths *)
let rec compare_lengths l1 l2 =
  match l1, l2 with
    [],[] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | _::t1, _::t2 -> compare_lengths t1 t2;; 
    
(* Función nth *)
let rec nth l p =
  if p < 0
    then raise (Invalid_argument "nth")
  else
    match l with
      [] -> raise (Failure "nth")
    | h::t ->
      if p = 0
        then h
      else nth t (p-1);;  

(* Función append *)
let rec append l1 l2 =
  match l1 with
    [] -> l2
  | h::t -> h::append t l2;; (* No recursiva terminal! *)

(* SEGUNDA PARTE *)

(* Función find *)
let rec find c l=
  match l with
    [] -> raise (Not_found)
  | h::t ->
    if (c h)
      then h
    else find c t;;

(* Función for_all *)
let rec for_all c l =
  match l with
    [] -> true
  | h::t ->
    if c h
      then for_all c t
    else false;;

(* Función exists *)
let rec exists c l =
  match l with
    [] -> false
  | h::t ->
    if c h
      then true
    else exists c t;;

(* Función mem *)
let rec mem i l =
  match l with
    [] -> false
  | h::t -> i = h || mem i t;;

(* Función filter *)
let rec filter c =
  let rec aux laux = function
  | [] ->
    let rec rev aux = function
      | [] -> aux
      | h::t -> rev (h::aux) t in
    rev [] laux
  | h::t ->
    if c h
      then aux (h::laux) t
    else
      aux laux t in
  aux [];;

(* Función find_all *)
let find_all c =
  let rec aux laux = function
  | [] ->
    let rec rev aux = function
      | [] -> aux
      | h::t -> rev (h::aux) t in
    rev [] laux
  | h::t ->
    if c h
      then aux (h::laux) t
    else
      aux laux t in
  aux [];;

(* Función partition *)
let rec partition c l =
  let rec aux cumple no_cumple = function
    | [] ->
      let rec rev aux = function
        | [] -> aux
        | h::t -> rev (h::aux) t in
      (rev [] cumple, rev [] no_cumple)
    | h::t ->
      if c h
        then aux (h::cumple) no_cumple t
      else
        aux cumple (h::no_cumple) t in
    aux [] [] l;;

(* Función split *)
let rec split l =
  match l with
    [] -> ([],[])
  | h::t ->
    (fst h::fst(split t), snd h::snd(split t));; (* No recursiva terminal! *)

(* Función combine *)
let rec combine l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (h1::t1, h2::t2) -> (h1,h2)::(combine t1 t2)
  | ([], _::_) | (_::_, []) ->
    raise (Invalid_argument "combine");; (* No recursiva terminal *)

(* TERCERA PARTE *)

(* Función init *)
let rec init len f =
  if len < 0
    then raise (Invalid_argument ("init"))
  else 
    let rec aux_init aux cnt n f =
      if cnt >= n
        then aux
      else
        aux_init ((f cnt)::aux) (cnt+1) n f in
      let rec rev aux = function
          [] -> aux
        | h::t -> rev (h::aux) t in
      rev [] (aux_init [] 0 len f);;

(* Función rev *)
let rev l =
  let rec aux_rev aux = function
      [] -> aux
    | h::t -> aux_rev (h::aux) t in
  aux_rev [] l;;

(* Función rev_append *)
let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | h::t -> rev_append t (h::l2);;
  
(* Función concat *)
let rec concat = function
  [] -> []
  | hd::tl ->
    let rec aux_concat l1 l2 =
      match l1 with
        [] -> l2
      | h::t -> h::aux_concat t l2 in
    aux_concat hd (concat tl);; (* No recursiva terminal *)
    
(* Función flatten *)
let rec flatten = function
  [] -> []
  | hd::tl ->
    let rec aux_flatten l1 l2 =
      match l1 with
        [] -> l2
      | h::t -> h::aux_flatten t l2 in
    aux_flatten hd (flatten tl);; (* No recursiva terminal *)

(* Función map *)
let rec map f l =
  match l with
    [] -> []
  | h::t -> (f h)::(map f t);; (* No recursiva terminal *)

(* Función rev_map *)
let rec rev_map f =
  let rec aux_rev aux = function
      [] -> aux
    | h::t -> aux_rev ((f h)::aux) t in
  aux_rev [];;

(* Función map2 *)
let rec map2 f l1 l2 =
  match (l1,l2) with
    ([],[]) -> []
  | (_::_, []) | ([], _::_) -> raise (Invalid_argument "map2")
  | (h1::t1, h2::t2) -> (f h1 h2)::(map2 f t1 t2);; (* No recursiva terminal *)

(* Función fold_left *)
let rec fold_left f a l =
  match l with
    [] -> a
  | h::t -> fold_left f (f a h) t;;

(* Función fold_right *)
let rec fold_right f l a =
  match l with
    [] -> a
  | h::t -> f h (fold_right f t a);; (* No recursiva terminal *)