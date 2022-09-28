open BinPos
open Exprdef
open List_lemma

type coq_Local =
| Temp of ident * expr_val

(** val coq_Local_eqb : coq_Local -> coq_Local -> bool **)

let coq_Local_eqb l1 l2 =
  let Temp (id, v) = l1 in
  let Temp (id1, v1) = l2 in (&&) (Pos.eqb id id1) (eqb_val v v1)

(** val eqb_Local_list : coq_Local list -> coq_Local list -> bool **)

let eqb_Local_list l1 l2 =
  eqb_list coq_Local_eqb l1 l2

(** val eval_tmpval : coq_Local list -> ident -> expr_val option **)

let rec eval_tmpval l x =
  match l with
  | [] -> None
  | h :: l' ->
    let Temp (x1, y1) = h in
    if Pos.eqb x1 x then Some y1 else eval_tmpval l' x

(** val coq_Changeval_Local :
    expr_val -> expr_val -> coq_Local list -> coq_Local list **)

let rec coq_Changeval_Local x y l = match l with
| [] -> l
| h :: l' ->
  let Temp (id, x') = h in
  (Temp (id, (coq_Changeval x y x'))) :: (coq_Changeval_Local x y l')
