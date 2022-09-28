open BinPos
open Exprdef
open List_lemma

type coq_Local =
| Temp of ident * expr_val

val coq_Local_eqb : coq_Local -> coq_Local -> bool

val eqb_Local_list : coq_Local list -> coq_Local list -> bool

val eval_tmpval : coq_Local list -> ident -> expr_val option

val coq_Changeval_Local :
  expr_val -> expr_val -> coq_Local list -> coq_Local list
