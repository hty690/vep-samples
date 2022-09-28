open BinNums
open BinPos

type ident = positive

val coq_Find : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> bool

val coq_Find_None : 'a1 option list -> bool

val coq_Clear_option : 'a1 option list -> 'a1 list

val coq_Find_A_in_prodAB :
  ('a1 -> 'a1 -> bool) -> ('a1 * 'a2) list -> 'a1 -> 'a2 option

val coq_Find_B_in_prodAB :
  ('a2 -> 'a2 -> bool) -> ('a1 * 'a2) list -> 'a2 -> 'a1 option

val remove_once : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> 'a1 list

val coq_Remove : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> 'a1 list

val coq_Same_part : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> 'a1 list

val coq_Remove_part : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> 'a1 list

val eqb_list : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val look_up : (ident * ident) list -> ident -> ident option

val coq_Add_ident_map : ident list -> ident -> (ident * ident) list

val coq_Rev_ident_list : (ident * ident) list -> (ident * ident) list
