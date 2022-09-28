open BinNums
open BinPos

type ident = positive

(** val coq_Find : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> bool **)

let rec coq_Find eqbA l a =
  match l with
  | [] -> false
  | b :: l' -> if eqbA a b then true else coq_Find eqbA l' a

(** val coq_Find_None : 'a1 option list -> bool **)

let rec coq_Find_None = function
| [] -> false
| a :: l' -> (match a with
              | Some _ -> coq_Find_None l'
              | None -> true)

(** val coq_Clear_option : 'a1 option list -> 'a1 list **)

let rec coq_Clear_option = function
| [] -> []
| a :: l' ->
  (match a with
   | Some a' -> a' :: (coq_Clear_option l')
   | None -> coq_Clear_option l')

(** val coq_Find_A_in_prodAB :
    ('a1 -> 'a1 -> bool) -> ('a1 * 'a2) list -> 'a1 -> 'a2 option **)

let rec coq_Find_A_in_prodAB eqbA l a =
  match l with
  | [] -> None
  | p :: l' ->
    let (a', b) = p in
    if eqbA a' a then Some b else coq_Find_A_in_prodAB eqbA l' a

(** val coq_Find_B_in_prodAB :
    ('a2 -> 'a2 -> bool) -> ('a1 * 'a2) list -> 'a2 -> 'a1 option **)

let rec coq_Find_B_in_prodAB eqbB l b =
  match l with
  | [] -> None
  | p :: l' ->
    let (a, b') = p in
    if eqbB b b' then Some a else coq_Find_B_in_prodAB eqbB l' b

(** val remove_once : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> 'a1 list **)

let rec remove_once eqbA l a =
  match l with
  | [] -> []
  | b :: l' -> if eqbA a b then l' else b :: (remove_once eqbA l' a)

(** val coq_Remove : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> 'a1 list **)

let rec coq_Remove eqbA l a =
  match l with
  | [] -> []
  | b :: l' ->
    if eqbA a b then coq_Remove eqbA l' a else b :: (coq_Remove eqbA l' a)

(** val coq_Same_part :
    ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> 'a1 list **)

let rec coq_Same_part eqbA l1 l2 =
  match l1 with
  | [] -> []
  | a :: l ->
    if coq_Find eqbA l2 a
    then a :: (coq_Same_part eqbA l (remove_once eqbA l2 a))
    else coq_Same_part eqbA l l2

(** val coq_Remove_part :
    ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> 'a1 list **)

let rec coq_Remove_part eqbA l1 = function
| [] -> l1
| a :: l2' -> coq_Remove_part eqbA (remove_once eqbA l1 a) l2'

(** val eqb_list : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec eqb_list eqbA l1 l2 =
  match l1 with
  | [] -> (match l2 with
           | [] -> true
           | _ :: _ -> false)
  | s :: l ->
    (&&) (coq_Find eqbA l2 s) (eqb_list eqbA l (remove_once eqbA l2 s))

(** val look_up : (ident * ident) list -> ident -> ident option **)

let look_up l a =
  coq_Find_A_in_prodAB Pos.eqb l a

(** val coq_Add_ident_map : ident list -> ident -> (ident * ident) list **)

let rec coq_Add_ident_map l a =
  match l with
  | [] -> []
  | b :: l' -> (b, (Pos.add b a)) :: (coq_Add_ident_map l' a)

(** val coq_Rev_ident_list : (ident * ident) list -> (ident * ident) list **)

let rec coq_Rev_ident_list = function
| [] -> []
| p :: l' -> let (b, c) = p in (c, b) :: (coq_Rev_ident_list l')
