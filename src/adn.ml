type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec build_list s n ls = 
    (*
       s: the string to explode
       n: the current index 
       (starts from the length of the string and goes down to 0)
       ls: the list to return
    *)
    if n<0 then ls
    else build_list s (n-1) ((String.get s n)::ls)
  in if str="" then [] else
  build_list str ((String.length str)-1) []

(* conversions *)
let base_of_char (c : char) : base =
  match c with
  |'A' -> A
  |'C' -> C
  |'G' -> G
  |'T' -> T
  |_ -> WC


let dna_of_string (s : string) : base list =
  let rec build_list lc ldna = 
    (*
      lc: la liste des caractères extraits de la chaine 
      ldna: la liste des bases DNA
    *)
    match lc with
    |[] -> List.rev ldna
    |a::r -> build_list r ((base_of_char a)::ldna)
  in build_list (explode s) []

let string_of_dna (seq : dna) : string =
  let rec build_string ldna s = 
    (*
      ldna: la liste des bases ADN
      s: la chaine a retourner
    *)
    match ldna with
    |[] -> s
    |d::r -> build_string r (s^(string_of_base d))
  in build_string seq ""

(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  let rec build_suffix lst pre = 
    (*
       lst: la liste compléte
       pre: le prefixe
    *)
    match lst with
    |[]->if pre=[] then Some [] else None
    |a::r -> if pre=[] then Some lst else
      if a = (List.hd pre) then build_suffix r (List.tl pre)
      else None
  in build_suffix list slice
(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)

let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  let rec build (lst: 'a list) (cmp: 'a list) (pre: 'a list) (pos: 'a list)= 
  (*
     lst: la liste complète
     cmp: la liste recherchée
     pre: préfixe de la liste
     suf: suffixe
     pos: une liste pour contenir les éléments de la liste cmp une fois trouvés dans lst
  *)
    match lst with
    |[] -> if cmp = [] then Some (pre, []) else None
    |bs::r -> if cmp=[] then Some (pre, lst)
    else 
      if bs=(List.hd cmp) then build r (List.tl cmp) (pre) (pos@[bs])
      else if cmp=slice then build r cmp (pre@[bs]) []
      else build lst slice (pre@pos) []
  in build list slice [] []
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let slices_between (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let rec aux l acc =
    match (first_occ start l) with 
    |None -> acc
    |Some (x, y) -> 
      match (first_occ stop y) with
      |None -> acc
      |Some (z, t) -> aux t (z::acc)
  in List.rev (aux list [])

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) =
  slices_between [A;T;G] [T;A;A] dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
let consensus (list : 'a list) : 'a consensus = 
  (*count the elements and add them in a list*)
  let count (lst: 'a list): (int list * 'a list) = 
    let rec build (lst: 'a list) (ints: int list) (alphas: 'a list) = 
      (*
      lst: list to process
      ints: counts for each *base*
      alphas: *bases*
      *)
      let rec count_one (lst: 'a list) (a: 'a): int = 
        match lst with 
        |[]->0
        |x::r -> if x=a then 1+(count_one r a)
        else count_one r a
      in
      match lst with
      |[]->(ints, alphas)
      |x::r -> if (List.mem x alphas) 
        then build r ints alphas
        else build r ((count_one lst x)::ints) (x::alphas)
    in build lst [] []

  and get_consensus ints alphas: ('a consensus) = 
    let rec find_cons (ints: int list) (alphas: 'a list) (cons: ('a consensus) option) (cons_base: 'a) (cons_count: int) = 
      match (ints, alphas) with
      |([], []) -> let finish cons =  
        match cons with 
        |None->No_consensus
        |Some cns -> cns
      in finish cons
      |(i::ri, a::ra) -> 
        (*si le nombre est null, alors on ne change pas le consensus*)
        if i=0 then find_cons ri ra cons cons_base cons_count 
        (* si le nombre est inférieure a la valeur max (mais pas null), on change de Full a Partial *)
        else if i<cons_count then match cons with
          |Some Full(_) -> find_cons ri ra (Some(Partial(cons_base, cons_count))) cons_base cons_count
          |Some Partial _ | Some No_consensus | None -> find_cons ri ra cons cons_base cons_count
        (*Si la valeur est égale a la valeur max, alors on change de Full (ou Partial) à No_consensus*)
        else if i=cons_count then 
          match cons with 
          |None-> find_cons ri ra None a i
          |Some _ -> find_cons ri ra (Some(No_consensus)) cons_base cons_count
        (*Sinon (si la valeur est suppérieure), on change le consensus de la façon suivante
            Full(ancien) -> Partial(Nouveau, valeur)
            Partial(ancien, val)->Partial(Nouveau, valeur)
            No_consensus -> Partial(Nouveau, valeur)
            None -> Full(nouveau)*)
        else 
          let f cons = match cons with
          |None->find_cons ri ra (Some(Full(a))) a i
          |Some _ -> find_cons ri ra (Some(Partial(a, i))) a i
        in f cons
        |(_, _)->failwith"Impossible"
    in find_cons ints alphas None (List.hd alphas) 0
      
  in match list with 
  |[] -> No_consensus
  |_ ->let pair = (count list) in (get_consensus (fst pair) (snd pair))

  (*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list = 
  let rec get_nths lst n ret = 
    match lst with
      |[] -> ret
      |a::r -> match (List.nth_opt a n) with 
      |None->[]
      |Some x -> get_nths r n (x::ret)
  in 
  let rec get_consensus_seq ll cnt len ret = 
    if cnt = len then List.rev ret
    else get_consensus_seq ll (cnt+1) len ((consensus (get_nths ll cnt []))::ret)
  in if ll=[] then [] else
  (get_consensus_seq ll 0 (List.length (List.hd ll)) [])
  
  (*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)