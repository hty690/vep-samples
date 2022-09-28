open BinPos
open Datatypes
open EXvar_help
open Localdef
open Propdef
open Exprdef
open List_lemma

type __ = Obj.t

type ('ptype, 't) coq_Separation =
| Coq_emp
| Data_at of expr_val * 'ptype * expr_val
| Memory of expr_val * expr_val * expr_val
| Time_spend of expr_val
| Other of 't

type ('rule, 'ptype, 't) coq_Proofrule =
| Same_cancel of ('ptype, 't) coq_Separation
| Time_cancel of expr_val * expr_val
| Fix_exist of ident * expr_val
| Others of 'rule

type 'e compare_checker_return =
| Coq_succ
| Coq_fail
| Coq_next of 'e

val coq_Rename_rule :
  (ident * ident) list -> ('a1, 'a2, 'a3) coq_Proofrule -> ('a1, 'a2, 'a3)
  coq_Proofrule

val coq_Rename_rule_list :
  (ident * ident) list -> ('a1, 'a2, 'a3) coq_Proofrule list -> ('a1, 'a2,
  'a3) coq_Proofrule list

type ('rule, 'ptype, 't) coq_Sep_solver_ret = { coq_Target_sep : ('ptype, 't)
                                                                 coq_Separation
                                                                 list;
                                                coq_Resource_sep : ('ptype,
                                                                   't)
                                                                   coq_Separation
                                                                   list;
                                                coq_EX_list : ident list;
                                                coq_Instantiation_map_list_sep : 
                                                (ident * expr_val) list;
                                                coq_Prop_list_sep : coq_Proposition
                                                                    list;
                                                coq_Proofrule_list : 
                                                ('rule, 'ptype, 't)
                                                coq_Proofrule list }

type 'ptype coq_Separation_def = { eqb_ptype : ('ptype -> 'ptype -> bool);
                                   eqb_sep : (__ -> __ -> bool);
                                   eqb_E : (__ -> __ -> bool);
                                   coq_Use_in_other : (__ -> expr_val -> bool);
                                   coq_Use_in_other_v : (__ -> expr_val ->
                                                        bool);
                                   coq_Use_in_other_address : (__ -> expr_val
                                                              -> bool);
                                   coq_Changeval_other : (expr_val ->
                                                         expr_val -> __ -> __);
                                   max_ident_T : (__ -> ident);
                                   is_emp : (__ -> bool);
                                   coq_Entailment_checker_with_EX : (__ ->
                                                                    ('ptype,
                                                                    __)
                                                                    coq_Separation
                                                                    -> ident
                                                                    list ->
                                                                    (__,
                                                                    'ptype,
                                                                    __)
                                                                    coq_Sep_solver_ret);
                                   coq_Sep_simplify_prechecker : (__ -> __
                                                                 option);
                                   coq_Get_prop : (__ -> __ ->
                                                  coq_Proposition list);
                                   coq_Get_simpl_expr_sep : (__ -> __
                                                            list * __ option);
                                   coq_Get_simpl_expr_data_at : (expr_val ->
                                                                'ptype ->
                                                                expr_val ->
                                                                __ option);
                                   coq_Sep_pure_fact_prechecker : (expr_val
                                                                  -> 'ptype
                                                                  -> expr_val
                                                                  -> __
                                                                  option);
                                   coq_Compare_sep : (__ -> ('ptype, __)
                                                     coq_Separation -> __
                                                     compare_checker_return);
                                   coq_Sep_split_prechecker : (__ -> __
                                                              option);
                                   coq_Sep_split_helper : (__ ->
                                                          coq_Proposition
                                                          list -> ident ->
                                                          ('ptype, __)
                                                          coq_Separation
                                                          list * ident list) }

type 'ptype coq_T = __

type 'ptype coq_E = __

type 'ptype rule = __

type 'ptype coq_Sep_solver_return =
  ('ptype rule, 'ptype, 'ptype coq_T) coq_Sep_solver_ret

val coq_Rename_other :
  'a1 coq_Separation_def -> 'a1 coq_T -> (ident * ident) list -> 'a1 coq_T

val coq_Find_ident_val_other :
  'a1 coq_Separation_def -> ident -> 'a1 coq_T -> bool

val coq_Separation_eqb :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation -> ('a1, 'a1
  coq_T) coq_Separation -> bool

val coq_Find_ident_s :
  'a1 coq_Separation_def -> ident -> ('a1, 'a1 coq_T) coq_Separation -> bool

val eqb_Sep_list :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1, 'a1
  coq_T) coq_Separation list -> bool

val coq_Change_sepx_typed :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> expr_val
  -> expr_val -> 'a1 -> ('a1, 'a1 coq_T) coq_Separation list * bool

val coq_Changeval_sep :
  'a1 coq_Separation_def -> expr_val -> expr_val -> ('a1, 'a1 coq_T)
  coq_Separation -> ('a1, 'a1 coq_T) coq_Separation

val coq_Changeval_sep_list :
  'a1 coq_Separation_def -> expr_val -> expr_val -> ('a1, 'a1 coq_T)
  coq_Separation list -> ('a1, 'a1 coq_T) coq_Separation list

val coq_Find_in_sep_address :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation -> expr_val ->
  bool

val coq_Find_in_Sep_list_address :
  'a1 coq_Separation_def -> expr_val -> ('a1, 'a1 coq_T) coq_Separation list
  -> ('a1, 'a1 coq_T) coq_Separation option

val coq_Rename_Separation :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation ->
  (ident * ident) list -> ('a1, 'a1 coq_T) coq_Separation

val coq_Rename_Separation_list :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list ->
  (ident * ident) list -> ('a1, 'a1 coq_T) coq_Separation list

val coq_Find_ident_S :
  'a1 coq_Separation_def -> ident -> ('a1, 'a1 coq_T) coq_Separation list ->
  bool

val coq_Find_ident_LS :
  'a1 coq_Separation_def -> ident -> coq_Local list -> ('a1, 'a1 coq_T)
  coq_Separation list -> bool

val coq_Compare_one :
  'a1 coq_Separation_def -> 'a1 coq_E -> ('a1, 'a1 coq_T) coq_Separation list
  -> ('a1, 'a1 coq_T) coq_Separation list -> 'a1 coq_E
  compare_checker_return * ('a1, 'a1 coq_T) coq_Separation list

val coq_Compare_one_no_succ :
  'a1 coq_Separation_def -> 'a1 coq_E -> ('a1, 'a1 coq_T) coq_Separation list
  -> ('a1, 'a1 coq_T) coq_Separation list -> 'a1 coq_E
  compare_checker_return * ('a1, 'a1 coq_T) coq_Separation list

val coq_Compare_n :
  'a1 coq_Separation_def -> 'a1 coq_E -> ('a1, 'a1 coq_T) coq_Separation list
  -> nat -> bool

val get_pure_fact_in_list :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1, 'a1
  coq_T) coq_Separation list -> coq_Proposition list

val coq_Get_simpl_expr :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1, 'a1
  coq_T) coq_Separation list -> 'a1 coq_E list

val coq_Sep_simplifier :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> 'a1 coq_E
  list -> nat -> ('a1, 'a1 coq_T) coq_Separation list * coq_Proposition list

val coq_Sep_split :
  'a1 coq_Separation_def -> coq_Proposition list -> ('a1, 'a1 coq_T)
  coq_Separation list -> ('a1, 'a1 coq_T) coq_Separation list -> ident ->
  ('a1, 'a1 coq_T) coq_Separation list * ident list

val all_emp_sep :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> bool

val coq_Sep_solver_with_EX :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation -> ('a1, 'a1
  coq_T) coq_Separation -> ident list -> 'a1 coq_Sep_solver_return

val coq_Sep_list_solver_with_EX_helper :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1, 'a1
  coq_T) coq_Separation list -> ident list -> 'a1 coq_Sep_solver_return

val coq_Sep_list_solver_with_EX_once :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1, 'a1
  coq_T) coq_Separation list -> ident list -> 'a1 coq_Sep_solver_return

val coq_Sep_list_solver_with_EX :
  'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1, 'a1
  coq_T) coq_Separation list -> ident list -> nat -> 'a1 coq_Sep_solver_return
