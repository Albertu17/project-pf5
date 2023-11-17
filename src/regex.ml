open Regex_base

(* À voir pour récursivité terminale ?*)
let repeat n l =
  if l = [] then [] else (* Si la liste à répéter est vide, on renvoie la liste vide. *)
  let rec aux m acc =
    if m = 0 then acc
    else l@(aux (m-1) acc)
  in aux n []

(* À simplifier *)
(* let expr_repeat n e =
  if n<=0 then Eps else
  (* Concat(e, expr_repeat (n-1) e) *) (* Cette version ajoute des concaténations inutiles. *)
  let rec aux n e k = 
    if n=1 then e else match e with
      | Eps | Star _ -> k e
      | Base _ | Joker | Concat _ | Alt _ -> aux (n-1) e (fun res -> k (Concat(e, res)))
      (* Concat(e, expr_repeat (n-1) e) *)
  in aux n e Fun.id *)

let rec expr_repeat n e =
  if n<=0 then Eps else
  if n=1 then e else match e with
    | Eps | Star _ -> e
    | Base _ | Joker | Concat _ | Alt _ -> Concat(e, expr_repeat (n-1) e)

(* let rec is_empty e = match e with
  | Eps -> true
  | Joker | Base _ -> false
  | Star a -> is_empty a
  | Concat(e1,e2) | Alt(e1,e2) -> (is_empty e1) && (is_empty e2) *)

(* Renvoie true si e reconnaît uniquement le mot vide, false sinon. *)
(* Fonction récursive terminale grâce au style par continuation. *)
let is_empty e = 
  let rec aux e k = match e with
    | Eps -> k true
    | Joker | Base _ -> k false
    | Star a -> k (aux a Fun.id)
    | Concat(e1,e2) | Alt(e1,e2) -> aux e1 (fun empty_e1 ->
                                    aux e2 (fun empty_e2 -> k (empty_e1 && empty_e2)))
  in aux e Fun.id

(* Renvoie true si e reconnaît le mot vide, false sinon. *)
(* Fonction récursive terminale grâce au style par continuation. *)
let null e = 
  let rec aux e k = match e with
    | Eps | Star _ -> k true
    | Base _ | Joker -> k false
    | Concat(e1,e2) ->  aux e1 (fun null_e1 ->
                        aux e2 (fun null_e2 -> k (null_e1 && null_e2)))
    | Alt(e1,e2) -> aux e1 (fun null_e1 ->
                    aux e2 (fun null_e2 -> k (null_e1 || null_e2)))
  in aux e Fun.id

(* Renvoie true si le langage reconnu par e est fini. *)
(* Fonction récursive terminale grâce au style par continuation. *)
let is_finite e = 
  let rec aux e k = match e with
    | Eps | Base _ | Joker -> k true
    | Star e -> k (is_empty e)
    | Concat(e1,e2) | Alt(e1,e2) -> aux e1 (fun finite_e1 ->
                                    aux e2 (fun finite_e2 -> k (finite_e1 && finite_e2)))
  in aux e Fun.id

(* Fonction récursive terminale: les fonctions List.fold_left et List.append (@) le sont. *)
let product l1 l2 =
  List.fold_left (fun x mot_l1 -> List.fold_left (fun y mot_l2 -> (mot_l1@mot_l2)::y) x l2) [] l1
  (* On peut éventuellement appliquer sort_uniq au résultat pour retourner une liste triée. *)

(* Prend en argument deux 'a list option et une fonction f, et
applique f aux deux 'a list option seulement si elles contiennent 
une liste, ie seulement si elles sont de la forme Some l. *)
let decide_to_apply f l1_opt l2_opt = match l1_opt, l2_opt with
  | Some _, None -> l1_opt
  | None, Some _ -> l1_opt
  | Some l1, Some l2 -> Some (f l1 l2)
  | None, None -> None

(* Transforme une liste en une liste de singletons, en remplaçant
chaque élément de la liste par son singleton correspondant. *)
let elements_to_singletons lst =
  let rec aux l acc = match l with
    | [] -> acc
    | h::t -> aux t ([h]::acc)
  in aux lst []

(* e est une expression sur l'ensemble fini de lettres alphabet. Si e reconnaît
un langage fini, renvoie Some de ce langage (ensemble de mots), sinon renvoie None *)
(* Fonction récursive terminale grâce au style par continuation. *)
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
                      aux e2 (fun langage_e2 -> k (decide_to_apply (@) langage_e1 langage_e2)))
  in aux exp Fun.id

(* Renvoie l'ensemble (liste triée sans duplicata) des lettres apparaissant dans e. *)
(* Fonction récursive terminale grâce au style par continuation. *)
let alphabet_expr e =
  let rec aux e acc k = match e with
    | Eps | Joker -> k acc
    | Base a -> k (a::acc)
    | Star e -> k (aux e acc Fun.id)
    | Concat(e1,e2) | Alt(e1,e2) -> aux e1 acc (fun alphabet_e1 ->
                                    aux e2 acc (fun alphabet_e2 -> k alphabet_e1@alphabet_e2))
  in sort_uniq (aux e [] Fun.id)

type answer =
  Infinite | Accept | Reject

let accept_partial e w = 
  let alphabet_e = alphabet_expr e in
  let alphabet_total = alphabet_e@w in
  match enumerate alphabet_total e with
    | Some l -> if List.mem w l then Accept else Reject
    | None -> Infinite
