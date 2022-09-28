open BinNums
open Datatypes
open Exprdef
open List_lemma

type __ = Obj.t

val coq_Up_eqb : __ -> bool

type coq_Binary_prop_op =
| Por
| Pand
| Pimply
| Piff

val coq_Bp_eqb : coq_Binary_prop_op -> coq_Binary_prop_op -> bool

type coq_Unary_expr_op =
| Pisptr
| Pis_pointer_or_null

val coq_Ue_eqb : coq_Unary_expr_op -> coq_Unary_expr_op -> bool

type coq_Binary_expr_op =
| Pvle
| Pvge
| Pvlt
| Pvgt
| Pvequal
| Psvle
| Psvge
| Psvlt
| Psvgt
| Share

val coq_Get_not_be_op : coq_Binary_expr_op -> coq_Binary_expr_op option

val coq_Get_neg_be_op : coq_Binary_expr_op -> coq_Binary_expr_op option

val cancel_op : coq_Binary_expr_op -> bool

val coq_Be_eqb : coq_Binary_expr_op -> coq_Binary_expr_op -> bool

type coq_Quantifier =
| PForall
| PExists

val coq_Quantifier_eqb : coq_Quantifier -> coq_Quantifier -> bool

type coq_Proposition =
| TT
| FF
| Bot
| Up of coq_Proposition
| Bp of coq_Binary_prop_op * coq_Proposition * coq_Proposition
| Ue of coq_Unary_expr_op * expr_val
| Be of coq_Binary_expr_op * expr_val * expr_val
| In_bound of expr_val * expr_val * expr_val
| Qf of coq_Quantifier * expr_val * coq_Proposition

val coq_Proposition_eqb : coq_Proposition -> coq_Proposition -> bool

val coq_Find_Prop_in_list : coq_Proposition list -> coq_Proposition -> bool

val coq_Simple_up_op : coq_Proposition -> coq_Proposition

val coq_Simple_be_op :
  coq_Binary_expr_op -> expr_val -> expr_val -> coq_Proposition

val coq_Simple_ue_op : coq_Unary_expr_op -> expr_val -> coq_Proposition

val coq_Main_bounded : expr_val -> expr_val

val coq_Simplify_prop : coq_Proposition -> coq_Proposition

val simpl_Proposition_list_1 : coq_Proposition list -> coq_Proposition list

val coq_Split_and_iff_bound_one : coq_Proposition -> coq_Proposition list

val coq_Split_and_iff_bound : coq_Proposition list -> coq_Proposition list

val simpl_Proposition_list : coq_Proposition list -> coq_Proposition list

val coq_Changeval_prop :
  expr_val -> expr_val -> coq_Proposition -> coq_Proposition

val coq_Changeval_prop_list :
  expr_val -> expr_val -> coq_Proposition list -> coq_Proposition list

val coq_Find_prop_in_used :
  expr_val -> coq_Proposition -> coq_Proposition option

val coq_Find_prop_in_used_list :
  expr_val -> coq_Proposition list -> coq_Proposition option

val coq_Change_prop_vequal :
  coq_Proposition list -> expr_val -> expr_val -> coq_Proposition list
