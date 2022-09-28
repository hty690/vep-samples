open AssSolver
open Assdef
open BinPos
open Datatypes
open EXvar_help
open List
open Localdef
open PeanoNat
open Sepdef
open Exprdef
open List_lemma

type 'ptype funcdes = { coq_Args : ident list; coq_Param : ident list;
                        coq_Precon : 'ptype coq_Prod_assert;
                        coq_Postcon : 'ptype coq_Prod_ret }

type 'ptype funcspec = ident * 'ptype funcdes list

val coq_Change_Val_option :
  'a1 coq_Separation_def -> (ident * expr_val) list -> expr_val option ->
  expr_val option

val list_Changeval_option :
  'a1 coq_Separation_def -> (expr_val * expr_val) list -> expr_val option ->
  expr_val option

val coq_Changeval_list_ret :
  'a1 coq_Separation_def -> (ident * expr_val) list -> 'a1 coq_Prod_ret ->
  'a1 coq_Prod_ret

val coq_Look_val :
  'a1 coq_Separation_def -> ident list -> 'a1 coq_Prod_assert -> expr_val list

val list_Changeval :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> (expr_val * expr_val) list
  -> 'a1 coq_Prod_assert

val list_Changeval_ret :
  'a1 coq_Separation_def -> (expr_val * expr_val) list -> 'a1 coq_Prod_ret ->
  'a1 coq_Prod_ret

val coq_Substitution_func :
  'a1 coq_Separation_def -> 'a1 funcdes -> expr_val list -> 'a1
  coq_Prod_assert * 'a1 coq_Prod_ret

val coq_New_name_func :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 funcdes -> 'a1 funcdes

val coq_Merge_Assert :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert -> 'a1
  coq_Prod_assert

val coq_Clear_Local :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert

val coq_Fill_logical_list :
  'a1 coq_Separation_def -> 'a1 coq_Prod_ret -> ident list ->
  (ident * expr_val) list -> 'a1 coq_Prod_ret option

val coq_Change_for_func :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 funcdes -> expr_val
  option list -> 'a1 coq_Prod_ret

val coq_Suitable :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 funcdes -> expr_val
  option list -> bool

val coq_Change_for_func_list :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 funcdes list ->
  expr_val option list -> 'a1 coq_Prod_ret list
