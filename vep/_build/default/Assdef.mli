open BinNums
open BinPos
open Datatypes
open EXvar_help
open Error
open Localdef
open Propdef
open Sepdef
open Exprdef
open List_lemma

type 'ptype assertion =
| Anormal of coq_Proposition list * coq_Local list
   * ('ptype, 'ptype coq_T) coq_Separation list
| Aex of ident * 'ptype assertion

type 'ptype coq_Prod_assert = { coq_Prop_list : coq_Proposition list;
                                coq_Local_list : coq_Local list;
                                coq_Sep_list : ('ptype, 'ptype coq_T)
                                               coq_Separation list;
                                coq_Exist_list : ident list }

type 'ptype coq_Prod_ret = { coq_Assert_r : 'ptype coq_Prod_assert;
                             coq_Return : expr_val option }

type 'ptype coq_Prod_error = { coq_Assert_e : 'ptype coq_Prod_assert;
                               coq_Errorm : coq_Error }

type 'ptype coq_Assert_return = { coq_Target_assert : 'ptype coq_Prod_assert;
                                  coq_Resource_assert : 'ptype coq_Prod_assert;
                                  instantiation_map_assert : (ident * expr_val)
                                                             list;
                                  coq_Proofrule_listx : ('ptype rule, 'ptype,
                                                        'ptype coq_T)
                                                        coq_Proofrule list }

val coq_Add_exist_list :
  'a1 coq_Separation_def -> 'a1 assertion -> ident list -> 'a1 assertion

val coq_Union_assert :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 assertion

val coq_Split_assert :
  'a1 coq_Separation_def -> 'a1 assertion -> 'a1 coq_Prod_assert

val coq_Changeval_Assert :
  'a1 coq_Separation_def -> expr_val -> expr_val -> 'a1 coq_Prod_assert ->
  'a1 coq_Prod_assert

val coq_Changeval_assertion :
  'a1 coq_Separation_def -> expr_val -> expr_val -> 'a1 assertion -> 'a1
  assertion

val coq_Clear_Prod_error :
  'a1 coq_Separation_def -> ('a1 assertion, 'a1 coq_Prod_error) sum list ->
  'a1 assertion list

val clear_unuse : coq_Proposition list -> ident -> coq_Proposition list

val coq_Clear_unuse :
  'a1 coq_Separation_def -> ident list -> coq_Proposition list -> coq_Local
  list -> ('a1, 'a1 coq_T) coq_Separation list -> coq_Proposition
  list * ident list

val coq_Clear_unuse_ass :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert

val coq_Clear_unuse_ret :
  'a1 coq_Separation_def -> 'a1 coq_Prod_ret -> 'a1 coq_Prod_ret

val coq_Rename_Ass :
  'a1 coq_Separation_def -> (ident * ident) list -> 'a1 coq_Prod_assert ->
  'a1 coq_Prod_assert

val coq_Rename_ret :
  'a1 coq_Separation_def -> (ident * ident) list -> 'a1 coq_Prod_ret -> 'a1
  coq_Prod_ret

val coq_Rename_instantiation_map :
  (ident * ident) list -> (ident * expr_val) list -> (ident * expr_val) list

val coq_Rename_return :
  'a1 coq_Separation_def -> (ident * ident) list -> 'a1 coq_Assert_return ->
  'a1 coq_Assert_return

val instantiation_pair_eqb : (ident * expr_val) -> (ident * expr_val) -> bool

val remove_duplicates :
  (ident * expr_val) list -> nat -> (ident * expr_val) list

val coq_Change_Val :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> (ident * expr_val) list ->
  'a1 coq_Prod_assert

val max_ident_in_prop : coq_Proposition -> ident

val max_ident_in_prop_list : coq_Proposition list -> ident

val max_ident_in_local : coq_Local list -> ident

val max_ident_in_sep :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ident

val max_ident_in_assertion :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> ident

val coq_Ass_unification :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert -> 'a1
  coq_Prod_assert

val coq_Return_back :
  'a1 coq_Separation_def -> 'a1 coq_Assert_return -> 'a1 coq_Prod_assert ->
  'a1 coq_Prod_assert -> 'a1 coq_Assert_return
