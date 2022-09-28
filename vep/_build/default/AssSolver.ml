open Assdef
open Datatypes
open Localdef
open Localsolver
open Nat
open Propdef
open Propsolver
open Sepdef

(** val coq_Ass_solve_new :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert ->
    coq_Proposition list -> 'a1 coq_E list -> nat -> 'a1 coq_Assert_return **)

let rec coq_Ass_solve_new sepT target resource last_pure_fact last_simpl_expr fuel =
  let l_return =
    coq_Local_list_solver_with_EX target.coq_Local_list
      resource.coq_Local_list target.coq_Exist_list
  in
  let new_Target =
    coq_Change_Val sepT { coq_Prop_list = target.coq_Prop_list;
      coq_Local_list = l_return.coq_Target_local; coq_Sep_list =
      target.coq_Sep_list; coq_Exist_list = target.coq_Exist_list }
      l_return.coq_Instantiation_map_list_local
  in
  let simpl_expr =
    app (coq_Get_simpl_expr sepT [] new_Target.coq_Sep_list) last_simpl_expr
  in
  let pure_fact =
    app last_pure_fact (get_pure_fact_in_list sepT [] resource.coq_Sep_list)
  in
  let max_ident = max_ident_in_assertion sepT new_Target in
  let s_split_return =
    coq_Sep_split sepT pure_fact new_Target.coq_Sep_list
      resource.coq_Sep_list max_ident
  in
  let s_return =
    coq_Sep_list_solver_with_EX sepT (fst s_split_return)
      resource.coq_Sep_list
      (app new_Target.coq_Exist_list (snd s_split_return))
      (length (fst s_split_return))
  in
  let instantiation_map =
    app l_return.coq_Instantiation_map_list_local
      s_return.coq_Instantiation_map_list_sep
  in
  let instantiation_map' =
    remove_duplicates instantiation_map (length instantiation_map)
  in
  let new_Target' =
    coq_Change_Val sepT { coq_Prop_list = new_Target.coq_Prop_list;
      coq_Local_list = l_return.coq_Target_local; coq_Sep_list =
      s_return.coq_Target_sep; coq_Exist_list = s_return.coq_EX_list }
      instantiation_map'
  in
  let target_Prop =
    app new_Target'.coq_Prop_list
      (app l_return.coq_Prop_list_local s_return.coq_Prop_list_sep)
  in
  let prop_return = coq_Prop_list_solver target_Prop resource.coq_Prop_list in
  (match prop_return with
   | Some prop_return_list ->
     if (&&)
          ((&&)
            ((&&)
              (eqb_Local_list target.coq_Local_list l_return.coq_Target_local)
              (eqb_Local_list resource.coq_Local_list
                l_return.coq_Resource_local))
            (eqb_Sep_list sepT target.coq_Sep_list s_return.coq_Target_sep))
          (eqb_Sep_list sepT resource.coq_Sep_list s_return.coq_Resource_sep)
     then { coq_Target_assert = { coq_Prop_list = prop_return_list;
            coq_Local_list = l_return.coq_Target_local; coq_Sep_list =
            s_return.coq_Target_sep; coq_Exist_list = s_return.coq_EX_list };
            coq_Resource_assert = { coq_Prop_list = resource.coq_Prop_list;
            coq_Local_list = l_return.coq_Resource_local; coq_Sep_list =
            s_return.coq_Resource_sep; coq_Exist_list =
            resource.coq_Exist_list }; instantiation_map_assert =
            instantiation_map'; coq_Proofrule_listx =
            s_return.coq_Proofrule_list }
     else (match fuel with
           | O ->
             { coq_Target_assert = { coq_Prop_list = prop_return_list;
               coq_Local_list = l_return.coq_Target_local; coq_Sep_list =
               s_return.coq_Target_sep; coq_Exist_list =
               s_return.coq_EX_list }; coq_Resource_assert =
               { coq_Prop_list = resource.coq_Prop_list; coq_Local_list =
               l_return.coq_Resource_local; coq_Sep_list =
               s_return.coq_Resource_sep; coq_Exist_list =
               resource.coq_Exist_list }; instantiation_map_assert =
               instantiation_map'; coq_Proofrule_listx =
               s_return.coq_Proofrule_list }
           | S fuel' ->
             let (sep_simpl, prop_from_simpl) =
               coq_Sep_simplifier sepT new_Target'.coq_Sep_list simpl_expr
                 (length new_Target'.coq_Sep_list)
             in
             let a_return =
               coq_Ass_solve_new sepT { coq_Prop_list =
                 (app prop_return_list prop_from_simpl); coq_Local_list =
                 new_Target'.coq_Local_list; coq_Sep_list = sep_simpl;
                 coq_Exist_list = new_Target'.coq_Exist_list }
                 { coq_Prop_list = resource.coq_Prop_list; coq_Local_list =
                 l_return.coq_Resource_local; coq_Sep_list =
                 s_return.coq_Resource_sep; coq_Exist_list =
                 resource.coq_Exist_list } pure_fact simpl_expr fuel'
             in
             { coq_Target_assert = a_return.coq_Target_assert;
             coq_Resource_assert = a_return.coq_Resource_assert;
             instantiation_map_assert =
             (app instantiation_map' a_return.instantiation_map_assert);
             coq_Proofrule_listx =
             (app s_return.coq_Proofrule_list a_return.coq_Proofrule_listx) })
   | None ->
     { coq_Target_assert = { coq_Prop_list = target_Prop; coq_Local_list =
       l_return.coq_Target_local; coq_Sep_list = s_return.coq_Target_sep;
       coq_Exist_list = s_return.coq_EX_list }; coq_Resource_assert =
       { coq_Prop_list = resource.coq_Prop_list; coq_Local_list =
       l_return.coq_Resource_local; coq_Sep_list = s_return.coq_Resource_sep;
       coq_Exist_list = resource.coq_Exist_list }; instantiation_map_assert =
       instantiation_map'; coq_Proofrule_listx = s_return.coq_Proofrule_list })

(** val coq_Ass_solver :
    'a1 coq_Separation_def -> 'a1 assertion -> 'a1 assertion -> 'a1
    coq_Assert_return **)

let coq_Ass_solver sepT target resource =
  let prod_T = coq_Split_assert sepT target in
  let prod_R = coq_Split_assert sepT resource in
  let t' = coq_Ass_unification sepT prod_T prod_R in
  let a_return =
    coq_Ass_solve_new sepT { coq_Prop_list = t'.coq_Prop_list;
      coq_Local_list = t'.coq_Local_list; coq_Sep_list = t'.coq_Sep_list;
      coq_Exist_list = t'.coq_Exist_list } { coq_Prop_list =
      prod_R.coq_Prop_list; coq_Local_list = prod_R.coq_Local_list;
      coq_Sep_list = prod_R.coq_Sep_list; coq_Exist_list =
      prod_R.coq_Exist_list } [] []
      (add (length prod_T.coq_Local_list) (length prod_T.coq_Sep_list))
  in
  coq_Return_back sepT a_return prod_T prod_R

(** val coq_Ass_solver_bool :
    'a1 coq_Separation_def -> 'a1 assertion -> 'a1 assertion -> bool **)

let coq_Ass_solver_bool sepT target resource =
  let a_return = coq_Ass_solver sepT target resource in
  let re_solve =
    coq_Prop_list_solver a_return.coq_Target_assert.coq_Prop_list
      a_return.coq_Resource_assert.coq_Prop_list
  in
  (match re_solve with
   | Some l ->
     (match l with
      | [] ->
        (&&)
          ((&&) (eqb_Local_list a_return.coq_Target_assert.coq_Local_list [])
            (all_emp_sep sepT a_return.coq_Target_assert.coq_Sep_list))
          (all_emp_sep sepT a_return.coq_Resource_assert.coq_Sep_list)
      | _ :: _ -> false)
   | None -> false)

(** val coq_Partial_Ass_solver_bool :
    'a1 coq_Separation_def -> 'a1 assertion -> 'a1 assertion -> bool **)

let coq_Partial_Ass_solver_bool sepT resource target =
  let a_return = coq_Ass_solver sepT target resource in
  let re_solve =
    coq_Prop_list_solver a_return.coq_Target_assert.coq_Prop_list
      a_return.coq_Resource_assert.coq_Prop_list
  in
  (match re_solve with
   | Some l ->
     (match l with
      | [] ->
        (&&) (eqb_Local_list a_return.coq_Target_assert.coq_Local_list [])
          (all_emp_sep sepT a_return.coq_Target_assert.coq_Sep_list)
      | _ :: _ -> false)
   | None -> false)

(** val coq_Ass_list_solver_helper1 :
    'a1 coq_Separation_def -> 'a1 assertion -> 'a1 assertion list -> bool **)

let rec coq_Ass_list_solver_helper1 sepT target = function
| [] -> false
| x :: l' ->
  (||) (coq_Ass_solver_bool sepT target x)
    (coq_Ass_list_solver_helper1 sepT target l')

(** val coq_Ass_list_solver_helper2 :
    'a1 coq_Separation_def -> 'a1 assertion list -> 'a1 assertion -> bool **)

let rec coq_Ass_list_solver_helper2 sepT targetx resource =
  match targetx with
  | [] -> true
  | x :: l' ->
    (||) (coq_Ass_solver_bool sepT x resource)
      (coq_Ass_list_solver_helper2 sepT l' resource)

(** val coq_Ass_list_solver_bool1 :
    'a1 coq_Separation_def -> 'a1 assertion list -> 'a1 assertion list -> bool **)

let rec coq_Ass_list_solver_bool1 sepT targetx resourcex =
  match targetx with
  | [] -> true
  | xT :: tL' ->
    (&&) (coq_Ass_list_solver_helper1 sepT xT resourcex)
      (coq_Ass_list_solver_bool1 sepT tL' resourcex)

(** val coq_Ass_list_solver_bool2 :
    'a1 coq_Separation_def -> 'a1 assertion list -> 'a1 assertion list -> bool **)

let rec coq_Ass_list_solver_bool2 sepT targetx = function
| [] -> true
| xR :: rL' ->
  (&&) (coq_Ass_list_solver_helper2 sepT targetx xR)
    (coq_Ass_list_solver_bool2 sepT targetx rL')

(** val coq_Ass_list_solver :
    'a1 coq_Separation_def -> 'a1 assertion list -> 'a1 assertion list -> bool **)

let coq_Ass_list_solver sepT targetx resourcex =
  (&&) (coq_Ass_list_solver_bool1 sepT targetx resourcex)
    (coq_Ass_list_solver_bool2 sepT targetx resourcex)
