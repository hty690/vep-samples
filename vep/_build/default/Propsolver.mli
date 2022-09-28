open BinInt
open BinNums
open Datatypes
open Nat
open Propdef
open Exprdef
open List_lemma

val option_not : bool option -> bool option

val option_and : bool option -> bool option -> bool option

val option_or : bool option -> bool option -> bool option

val option_merge :
  (bool * expr_val option) -> (bool * expr_val option) -> bool * expr_val
  option

val coq_Check_for_vle : expr_val -> expr_val -> bool option

val coq_Check_for_vlt : expr_val -> expr_val -> bool option

val coq_Pvle_solve :
  expr_val -> expr_val -> coq_Proposition list -> nat -> bool option

val coq_Pvlt_solve :
  expr_val -> expr_val -> coq_Proposition list -> nat -> bool option

val coq_Pvge_solve :
  expr_val -> expr_val -> coq_Proposition list -> nat -> bool option

val coq_Pvgt_solve :
  expr_val -> expr_val -> coq_Proposition list -> nat -> bool option

val coq_Pvequal_solve :
  expr_val -> expr_val -> coq_Proposition list -> nat -> bool option

val coq_Pis_pointer_or_null_solve :
  expr_val -> coq_Proposition list -> nat -> bool option

val coq_Pisptr_solve : expr_val -> coq_Proposition list -> nat -> bool option

val coq_Similar_ignore_i_val :
  expr_val -> expr_val -> expr_val -> bool * expr_val option

val coq_Similar_ignore_i_prop :
  expr_val -> coq_Proposition -> coq_Proposition -> bool * expr_val option

val coq_Try_solve :
  coq_Proposition -> coq_Proposition -> bool * coq_Proposition option

val coq_Search_for_related :
  coq_Proposition -> coq_Proposition list -> coq_Proposition option

val coq_Divisor_reduct : coq_Proposition -> coq_Proposition

val coq_Prop_solve :
  coq_Proposition -> coq_Proposition list -> nat -> bool option

val coq_Prop_solver :
  coq_Proposition list -> coq_Proposition list -> nat -> coq_Proposition list
  option

val coq_Prop_list_solver :
  coq_Proposition list -> coq_Proposition list -> coq_Proposition list option
