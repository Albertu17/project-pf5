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
     lst: la liste compléte
     cmp: la liste recherché
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


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  match (first_occ start list) with 
  |None -> []
  |Some (x, y) -> 
    match (first_occ stop y) with
    |None -> []
    |Some (z, t) -> z::(slices_between start stop t)

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
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
  let rec build_transpose ll tr n = 
    "Nothing"
  and 
    count le a c g t =
    (*
      le: liste des éléments de la même colonne
      a: compteur des bases A
      c: compteur des bases C
      g: compteur des bases G
      t: compteur des bases T
    *)
    match le with
    |[] -> (a, c, g, t)
    |A::r -> count r (a+1) c g t
    |C::r -> count r a (c+1) g t
    |G::r -> count r a c (g+1) t
    |T::r -> count r a c  g (t+1)
    |_::r -> count r a c g t
   
  
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
  failwith "A completer"

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
