open Regex_base

(* À voir pour récursivité terminale ?*)
let repeat n l =
  if l = [] then [] else (* Si la liste à répéter est vide, on renvoie la liste vide. *)
  let rec aux m acc =
    if m = 0 then acc
    else l@(aux (m-1) acc)
  in aux n []

(* À simplifier *)
let rec expr_repeat n e =
  if n<=0 then Eps else
  Concat(e, expr_repeat (n-1) e) (* Cette version ajoute des concaténations inutiles. *)
  (* if n=1 then e else match e with
  | Eps | Star _ -> e
  | Base _ | Joker | Concat _ | Alt _ -> Concat(e, expr_repeat (n-1) e) *)

(* Renvoie true si e reconnaît uniquement le mot vide, false sinon. *)
let rec is_empty e = match e with
  | Eps -> true
  | Joker | Base _ -> false
  | Star a -> is_empty a
  | Concat(e1,e2) | Alt(e1,e2) -> (is_empty e1) && (is_empty e2)

(* Renvoie true si e reconnaît le mot vide, false sinon. *)
let rec null e = match e with
  | Eps | Star _ -> true
  | Base _ | Joker -> false
  | Concat(e1,e2) -> (null e1) && (null e2)
  | Alt(e1,e2) -> (null e1) || (null e2)

let rec is_finite e = match e with
  | Eps | Base _ | Joker -> true
  | Star e -> is_empty e
  | Concat(e1,e2) | Alt(e1,e2) -> (is_finite e1) && (is_finite e2)

let product l1 l2 =
  (* let rec parcours_l2 l acc = match l with
    | [] -> acc
    | h::t -> (parcours_l2 t' (acc'@[h;h']))
  in
  let rec parcours_l1 l acc = match l with
    | [] -> acc
    | h::t -> 
  in parcours_l1 l1 [] *)
  (* failwith "À compléter" *)

let enumerate alphabet e =
  failwith "À compléter"

(* À voir pour récursivité terminale ?*)
let alphabet_expr e =
  let rec aux acc exp = match exp with
    | Eps | Joker -> acc
    | Base a -> a::acc
    | Star e -> aux acc e
    | Concat(e1,e2) | Alt(e1,e2) -> (aux acc e1)@(aux acc e2)
  in sort_uniq (aux [] e)

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
