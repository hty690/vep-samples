open Assdef
open Datatypes
open Localdef
open Localsolver
open Nat
open Propdef
open Propsolver
open Sepdef

val coq_Ass_solve_new :
  'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert ->
  coq_Proposition list -> 'a1 coq_E list -> nat -> 'a1 coq_Assert_return

val coq_Ass_solver :
  'a1 coq_Separation_def -> 'a1 assertion -> 'a1 assertion -> 'a1
  coq_Assert_return

val coq_Ass_solver_bool :
  'a1 coq_Separation_def -> 'a1 assertion -> 'a1 assertion -> bool

val coq_Partial_Ass_solver_bool :
  'a1 coq_Separation_def -> 'a1 assertion -> 'a1 assertion -> bool

val coq_Ass_list_solver_helper1 :
  'a1 coq_Separation_def -> 'a1 assertion -> 'a1 assertion list -> bool

val coq_Ass_list_solver_helper2 :
  'a1 coq_Separation_def -> 'a1 assertion list -> 'a1 assertion -> bool

val coq_Ass_list_solver_bool1 :
  'a1 coq_Separation_def -> 'a1 assertion list -> 'a1 assertion list -> bool

val coq_Ass_list_solver_bool2 :
  'a1 coq_Separation_def -> 'a1 assertion list -> 'a1 assertion list -> bool

val coq_Ass_list_solver :
  'a1 coq_Separation_def -> 'a1 assertion list -> 'a1 assertion list -> bool
