open BinPos
open Datatypes
open Localdef
open Propdef
open Exprdef
open List_lemma

type coq_Local_solver_return = { coq_Localx : (coq_Local * coq_Local) option;
                                 coq_Instantiation_map : (ident * expr_val)
                                                         list;
                                 coq_PropL : coq_Proposition list }

type coq_Local_list_helper_return = { coq_Targetx : coq_Local option;
                                      coq_Resourcex : coq_Local list;
                                      coq_Instantiation_map' : (ident * expr_val)
                                                               list;
                                      coq_PropL' : coq_Proposition list }

type coq_Local_list_solver_return = { coq_Target_local : coq_Local list;
                                      coq_Resource_local : coq_Local list;
                                      coq_Instantiation_map_list_local : 
                                      (ident * expr_val) list;
                                      coq_Prop_list_local : coq_Proposition
                                                            list }

val coq_Local_solver_with_EX :
  coq_Local -> coq_Local -> ident list -> coq_Local_solver_return

val coq_Local_list_solver_with_EX_helper :
  coq_Local -> coq_Local list -> ident list -> nat ->
  coq_Local_list_helper_return

val coq_Local_list_solver_with_EX :
  coq_Local list -> coq_Local list -> ident list ->
  coq_Local_list_solver_return
