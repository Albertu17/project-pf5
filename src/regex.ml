open Regex_base

(* Toutes les fonctions de cette section sont récursives terminales. *)

(* Renvoie le mot w (liste de caractères) concaténé n fois avec lui-même. *)
let repeat n w =
  if w = [] then [] else (* Si la liste à répéter est vide, on renvoie la liste vide. *)
  let rec aux m acc =
    if m = 0 then acc
    else aux (m-1) (w@acc) (* fonction List.append (@) récursive terminale depuis OCaml 5.1. *)
  in aux n []

(* Renvoie une expression régulière qui reconnaît les mots formés de
la concaténation de n mots reconnus par e. *)
let expr_repeat n e =
  if n <= 0 then Eps else 
  let rec aux n expr_acc =
    if n=1 then expr_acc
    else aux (n-1) (Concat(e, expr_acc))
  in aux n e

(* Renvoie true si exp reconnaît uniquement le mot vide, false sinon. *)
(* Fonction récursive terminale par passage de continuations. *)
let is_empty exp = 
  let rec aux e k = match e with
    | Eps -> k true
    | Joker | Base _ -> k false
    | Star a -> k (aux a Fun.id)
    | Concat(e1,e2) | Alt(e1,e2) -> aux e1 (fun empty_e1 ->
                                    aux e2 (fun empty_e2 -> k (empty_e1 && empty_e2)))
  in aux exp Fun.id

(* Renvoie true si exp reconnaît le mot vide, false sinon. *)
(* Fonction récursive terminale par passage de continuations. *)
let null exp = 
  let rec aux e k = match e with
    | Eps | Star _ -> k true
    | Base _ | Joker -> k false
    | Concat(e1,e2) ->  aux e1 (fun null_e1 ->
                        aux e2 (fun null_e2 -> k (null_e1 && null_e2)))
    | Alt(e1,e2) -> aux e1 (fun null_e1 ->
                    aux e2 (fun null_e2 -> k (null_e1 || null_e2)))
  in aux exp Fun.id

(* Renvoie true si le langage reconnu par exp est fini. *)
(* Fonction récursive terminale par passage de continuations. *)
let is_finite exp = 
  let rec aux e k = match e with
    | Eps | Base _ | Joker -> k true
    | Star e -> k (is_empty e)
    | Concat(e1,e2) | Alt(e1,e2) -> aux e1 (fun finite_e1 ->
                                    aux e2 (fun finite_e2 -> k (finite_e1 && finite_e2)))
  in aux exp Fun.id

(* Renvoie l'ensemble des mots formés de la concaténation d'un mot de l1 et d'un mot de l2. *)
(* Fonction récursive terminale, les fonctions List.fold_left et List.append (@) le sont. *)
let product l1 l2 =
  List.fold_left (fun x mot_l1 -> List.fold_left (fun y mot_l2 -> (mot_l1@mot_l2)::y) x l2) [] l1
  (* Les parcours des listes l1 et l2 sont pris en charge par la fonction fold_left. *)

(* Prend en argument deux 'a list option et une fonction f, et
applique f au contenu des deux 'a list option seulement si elles 
sont de la forme Some l. *)
let decide_to_apply f l1_opt l2_opt = match l1_opt, l2_opt with
  | Some _, None -> l1_opt
  | None, Some _ -> l1_opt
  | Some l1, Some l2 -> Some (f l1 l2)
  | None, None -> None

(* Transforme une liste en une liste de singletons, en remplaçant
chaque élément par la liste de taille 1 le contenant. *)
let elements_to_singletons lst =
  let rec aux l acc = match l with
    | [] -> acc
    | h::t -> aux t ([h]::acc)
  in aux lst []

(* Si exp, expression sur l'ensemble fini de lettres alphabet, reconnaît
un langage fini l (ensemble de mots), renvoie Some l, sinon renvoie None *)
(* Fonction récursive terminale par passage de continuations. *)
let enumerate alphabet exp = 
  let rec aux e k = match e with
    | Eps -> k (Some [[]])
    | Base a -> k (Some [[a]])
    | Joker -> k (Some (elements_to_singletons alphabet)) (* Les mots reconnus par un joker 
                                                            sont les lettres. *)
    | Star e -> if is_empty e then k (Some [[]]) else k None
    | Concat(e1, e2) -> aux e1 (fun langage_e1 ->
                        aux e2 (fun langage_e2 -> k (decide_to_apply product langage_e1 langage_e2)))
    | Alt(e1, e2) ->  aux e1 (fun langage_e1 ->
                      aux e2 (fun langage_e2 -> k (decide_to_apply union_sorted langage_e1 langage_e2)))
  in aux exp Fun.id

(* Renvoie l'ensemble (liste triée sans duplicata) des lettres apparaissant dans l'expression exp. *)
(* Fonction récursive terminale par passage de continuations. *)
let alphabet_expr exp =
  let rec aux e acc k = match e with
    | Eps | Joker -> k acc
    | Base a -> k (a::acc)
    | Star e -> k (aux e acc Fun.id)
    | Concat(e1,e2) | Alt(e1,e2) -> aux e1 acc (fun alphabet_e1 ->
                                    aux e2 acc (fun alphabet_e2 -> k alphabet_e1@alphabet_e2))
  in sort_uniq (aux exp [] Fun.id)

type answer =
  Infinite | Accept | Reject

(* Renvoie Infinite si le langage reconnu par e est infini, Accept si le langage reconnu
par e est fini et contient le mot w, Reject si le langage reconnu par e est fini et ne
contient pas le mot w. *)
let accept_partial e w = 
  let alphabet_e = alphabet_expr e in
  let alphabet_total = alphabet_e@w in
  match enumerate alphabet_total e with
    | Some l -> if List.mem w l then Accept else Reject
    | None -> Infinite
    