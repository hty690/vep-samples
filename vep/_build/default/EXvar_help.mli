open BinNums
open BinPos
open Localdef
open Propdef
open Exprdef
open List_lemma

val max_ident : ident list -> ident

val coq_Rename : expr_val -> (ident * ident) list -> expr_val

val coq_Rename_option :
  expr_val option -> (ident * ident) list -> expr_val option

val coq_Rename_Proposition :
  coq_Proposition -> (ident * ident) list -> coq_Proposition

val coq_Rename_Local : coq_Local -> (ident * ident) list -> coq_Local

val coq_Rename_Proposition_list :
  coq_Proposition list -> (ident * ident) list -> coq_Proposition list

val coq_Rename_Local_list :
  coq_Local list -> (ident * ident) list -> coq_Local list

val coq_Rename_ident : ident list -> (ident * ident) list -> ident list

val coq_Find_ident_val : ident -> expr_val -> bool

val coq_Find_ident_l : ident -> coq_Local -> bool

val coq_Find_ident_L : ident -> coq_Local list -> bool

val coq_Find_ident_p : ident -> coq_Proposition -> bool

val coq_Find_ident_P : ident -> coq_Proposition list -> bool
