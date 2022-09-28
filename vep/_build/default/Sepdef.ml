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

(** val coq_Rename_rule :
    (ident * ident) list -> ('a1, 'a2, 'a3) coq_Proofrule -> ('a1, 'a2, 'a3)
    coq_Proofrule **)

let coq_Rename_rule change_list r = match r with
| Time_cancel (t1, t2) ->
  Time_cancel ((coq_Rename t1 change_list), (coq_Rename t2 change_list))
| Fix_exist (id, v) ->
  (match look_up change_list id with
   | Some c -> Fix_exist (c, (coq_Rename v change_list))
   | None -> Fix_exist (id, (coq_Rename v change_list)))
| _ -> r

(** val coq_Rename_rule_list :
    (ident * ident) list -> ('a1, 'a2, 'a3) coq_Proofrule list -> ('a1, 'a2,
    'a3) coq_Proofrule list **)

let rec coq_Rename_rule_list change_list = function
| [] -> []
| r0 :: r' ->
  (coq_Rename_rule change_list r0) :: (coq_Rename_rule_list change_list r')

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

(** val coq_Rename_other :
    'a1 coq_Separation_def -> 'a1 coq_T -> (ident * ident) list -> 'a1 coq_T **)

let rec coq_Rename_other sepT t = function
| [] -> t
| p :: l' ->
  let (a, b) = p in
  coq_Rename_other sepT (sepT.coq_Changeval_other (V_vari a) (V_vari b) t) l'

(** val coq_Find_ident_val_other :
    'a1 coq_Separation_def -> ident -> 'a1 coq_T -> bool **)

let coq_Find_ident_val_other sepT id t =
  sepT.coq_Use_in_other t (V_vari id)

(** val coq_Separation_eqb :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation -> ('a1, 'a1
    coq_T) coq_Separation -> bool **)

let coq_Separation_eqb sepT s1 s2 =
  match s1 with
  | Coq_emp -> (match s2 with
                | Coq_emp -> true
                | _ -> false)
  | Data_at (e, t, e') ->
    (match s2 with
     | Data_at (e1, t1, e1') ->
       (&&) ((&&) (eqb_val e e1) (eqb_val e' e1')) (sepT.eqb_ptype t t1)
     | _ -> false)
  | Memory (x1, len1, s3) ->
    (match s2 with
     | Memory (x2, len2, s4) ->
       (&&) ((&&) (eqb_val x1 x2) (eqb_val len1 len2)) (eqb_val s3 s4)
     | _ -> false)
  | Time_spend t1 ->
    (match s2 with
     | Time_spend t2 -> eqb_val t1 t2
     | _ -> false)
  | Other t1 -> (match s2 with
                 | Other t2 -> sepT.eqb_sep t1 t2
                 | _ -> false)

(** val coq_Find_ident_s :
    'a1 coq_Separation_def -> ident -> ('a1, 'a1 coq_T) coq_Separation -> bool **)

let coq_Find_ident_s sepT i = function
| Coq_emp -> false
| Data_at (v, _, address) ->
  (||) (coq_Find_ident_val i v) (coq_Find_ident_val i address)
| Memory (x, len, s0) ->
  (||) ((||) (coq_Find_ident_val i x) (coq_Find_ident_val i len))
    (coq_Find_ident_val i s0)
| Time_spend t -> coq_Find_ident_val i t
| Other t -> coq_Find_ident_val_other sepT i t

(** val eqb_Sep_list :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1,
    'a1 coq_T) coq_Separation list -> bool **)

let eqb_Sep_list sepT s1 s2 =
  eqb_list (coq_Separation_eqb sepT) s1 s2

(** val coq_Change_sepx_typed :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list ->
    expr_val -> expr_val -> 'a1 -> ('a1, 'a1 coq_T) coq_Separation list * bool **)

let rec coq_Change_sepx_typed sepT s x y ty =
  match s with
  | [] -> (s, false)
  | h :: s' ->
    let (ns, flag) = coq_Change_sepx_typed sepT s' x y ty in
    (match h with
     | Data_at (_, _, x2) ->
       if eqb_val x2 x
       then (((Data_at (y, ty, x2)) :: ns), true)
       else ((h :: ns), flag)
     | _ -> ((h :: ns), flag))

(** val coq_Changeval_sep :
    'a1 coq_Separation_def -> expr_val -> expr_val -> ('a1, 'a1 coq_T)
    coq_Separation -> ('a1, 'a1 coq_T) coq_Separation **)

let coq_Changeval_sep sepT x y s = match s with
| Coq_emp -> s
| Data_at (x1, t, x2) ->
  Data_at ((coq_Changeval x y x1), t, (coq_Changeval x y x2))
| Memory (x1, x2, s1) ->
  Memory ((coq_Changeval x y x1), (coq_Changeval x y x2), s1)
| Time_spend t -> Time_spend (coq_Changeval x y t)
| Other t -> Other (sepT.coq_Changeval_other x y t)

(** val coq_Changeval_sep_list :
    'a1 coq_Separation_def -> expr_val -> expr_val -> ('a1, 'a1 coq_T)
    coq_Separation list -> ('a1, 'a1 coq_T) coq_Separation list **)

let rec coq_Changeval_sep_list sepT x y = function
| [] -> []
| p :: s' ->
  (coq_Changeval_sep sepT x y p) :: (coq_Changeval_sep_list sepT x y s')

(** val coq_Find_in_sep_address :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation -> expr_val ->
    bool **)

let coq_Find_in_sep_address sepT s v =
  match s with
  | Data_at (_, _, address) -> eqb_val (get_val address) v
  | Memory (x, _, s0) -> (||) (eqb_val x v) (eqb_val s0 v)
  | Other t -> sepT.coq_Use_in_other_address t v
  | _ -> false

(** val coq_Find_in_Sep_list_address :
    'a1 coq_Separation_def -> expr_val -> ('a1, 'a1 coq_T) coq_Separation
    list -> ('a1, 'a1 coq_T) coq_Separation option **)

let rec coq_Find_in_Sep_list_address sepT v = function
| [] -> None
| s' :: l' ->
  if coq_Find_in_sep_address sepT s' v
  then Some s'
  else coq_Find_in_Sep_list_address sepT v l'

(** val coq_Rename_Separation :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation ->
    (ident * ident) list -> ('a1, 'a1 coq_T) coq_Separation **)

let coq_Rename_Separation sepT p f =
  match p with
  | Coq_emp -> p
  | Data_at (v, t, address) ->
    Data_at ((coq_Rename v f), t, (coq_Rename address f))
  | Memory (x, y, s) ->
    Memory ((coq_Rename x f), (coq_Rename y f), (coq_Rename s f))
  | Time_spend t -> Time_spend (coq_Rename t f)
  | Other t -> Other (coq_Rename_other sepT t f)

(** val coq_Rename_Separation_list :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list ->
    (ident * ident) list -> ('a1, 'a1 coq_T) coq_Separation list **)

let rec coq_Rename_Separation_list sepT sepx_list f =
  match sepx_list with
  | [] -> []
  | p :: l' ->
    (coq_Rename_Separation sepT p f) :: (coq_Rename_Separation_list sepT l' f)

(** val coq_Find_ident_S :
    'a1 coq_Separation_def -> ident -> ('a1, 'a1 coq_T) coq_Separation list
    -> bool **)

let rec coq_Find_ident_S sepT i = function
| [] -> false
| l :: l0 -> (||) (coq_Find_ident_s sepT i l) (coq_Find_ident_S sepT i l0)

(** val coq_Find_ident_LS :
    'a1 coq_Separation_def -> ident -> coq_Local list -> ('a1, 'a1 coq_T)
    coq_Separation list -> bool **)

let coq_Find_ident_LS sepT i localx sepx =
  (||) (coq_Find_ident_L i localx) (coq_Find_ident_S sepT i sepx)

(** val coq_Compare_one :
    'a1 coq_Separation_def -> 'a1 coq_E -> ('a1, 'a1 coq_T) coq_Separation
    list -> ('a1, 'a1 coq_T) coq_Separation list -> 'a1 coq_E
    compare_checker_return * ('a1, 'a1 coq_T) coq_Separation list **)

let rec coq_Compare_one sepT check_expr sepList1 = function
| [] -> ((Coq_next check_expr), sepList1)
| s :: sepList2' ->
  let sepList1' = s :: sepList1 in
  (match sepT.coq_Compare_sep check_expr s with
   | Coq_succ -> (Coq_succ, [])
   | Coq_fail -> coq_Compare_one sepT check_expr sepList1' sepList2'
   | Coq_next check_expr' ->
     coq_Compare_one sepT check_expr' sepList1 sepList2')

(** val coq_Compare_one_no_succ :
    'a1 coq_Separation_def -> 'a1 coq_E -> ('a1, 'a1 coq_T) coq_Separation
    list -> ('a1, 'a1 coq_T) coq_Separation list -> 'a1 coq_E
    compare_checker_return * ('a1, 'a1 coq_T) coq_Separation list **)

let rec coq_Compare_one_no_succ sepT check_expr sepList1 = function
| [] -> (Coq_fail, [])
| s :: sepList2' ->
  let sepList1' = s :: sepList1 in
  (match sepT.coq_Compare_sep check_expr s with
   | Coq_succ -> (Coq_succ, [])
   | Coq_fail -> coq_Compare_one_no_succ sepT check_expr sepList1' sepList2'
   | Coq_next check_expr' ->
     coq_Compare_one sepT check_expr' sepList1 sepList2')

(** val coq_Compare_n :
    'a1 coq_Separation_def -> 'a1 coq_E -> ('a1, 'a1 coq_T) coq_Separation
    list -> nat -> bool **)

let rec coq_Compare_n sepT check_expr sepList = function
| O -> false
| S fuel' ->
  (match sepList with
   | [] -> false
   | _ :: _ ->
     let (c, sepList') = coq_Compare_one_no_succ sepT check_expr [] sepList in
     (match c with
      | Coq_succ -> true
      | Coq_fail -> false
      | Coq_next check_expr' -> coq_Compare_n sepT check_expr' sepList' fuel'))

(** val get_pure_fact_in_list :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1,
    'a1 coq_T) coq_Separation list -> coq_Proposition list **)

let rec get_pure_fact_in_list sepT sepList1 = function
| [] -> []
| s :: sepList2' ->
  let sepList1' = s :: sepList1 in
  (match s with
   | Data_at (v, type0, address) ->
     (match sepT.coq_Sep_pure_fact_prechecker v type0 address with
      | Some check_expr ->
        if coq_Compare_n sepT check_expr (app sepList2' sepList1)
             (length (app sepList2' sepList1))
        then (match address with
              | Vfield_address (addr, _) ->
                (Up (Be (Pvequal, v,
                  addr))) :: (get_pure_fact_in_list sepT sepList1' sepList2')
              | _ -> get_pure_fact_in_list sepT sepList1' sepList2')
        else get_pure_fact_in_list sepT sepList1' sepList2'
      | None -> get_pure_fact_in_list sepT sepList1' sepList2')
   | _ -> get_pure_fact_in_list sepT sepList1' sepList2')

(** val coq_Get_simpl_expr :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1,
    'a1 coq_T) coq_Separation list -> 'a1 coq_E list **)

let rec coq_Get_simpl_expr sepT sepList1 = function
| [] -> []
| s :: sepList2' ->
  let sepList1' = s :: sepList1 in
  (match s with
   | Data_at (v, type0, address) ->
     (match sepT.coq_Get_simpl_expr_data_at v type0 address with
      | Some e -> e :: (coq_Get_simpl_expr sepT sepList1' sepList2')
      | None -> coq_Get_simpl_expr sepT sepList1' sepList2')
   | Other t ->
     let (simpl_expr_list, o) = sepT.coq_Get_simpl_expr_sep t in
     (match simpl_expr_list with
      | [] -> coq_Get_simpl_expr sepT sepList1' sepList2'
      | _ :: _ ->
        (match o with
         | Some e ->
           if coq_Compare_n sepT e (app sepList2' sepList1)
                (length (app sepList2' sepList1))
           then app simpl_expr_list
                  (coq_Get_simpl_expr sepT sepList1' sepList2')
           else coq_Get_simpl_expr sepT sepList1' sepList2'
         | None ->
           app simpl_expr_list (coq_Get_simpl_expr sepT sepList1' sepList2')))
   | _ -> coq_Get_simpl_expr sepT sepList1' sepList2')

(** val coq_Sep_simplifier :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> 'a1
    coq_E list -> nat -> ('a1, 'a1 coq_T) coq_Separation
    list * coq_Proposition list **)

let rec coq_Sep_simplifier sepT sepList simpl_expr_list = function
| O -> (sepList, [])
| S fuel' ->
  (match sepList with
   | [] -> ([], [])
   | sep :: sepList' ->
     (match sep with
      | Other sepT0 ->
        (match sepT.coq_Sep_simplify_prechecker sepT0 with
         | Some check_expr ->
           if coq_Find sepT.eqb_E simpl_expr_list check_expr
           then let return_prop = sepT.coq_Get_prop sepT0 check_expr in
                ((fst
                   (coq_Sep_simplifier sepT sepList' simpl_expr_list fuel')),
                (app return_prop
                  (snd
                    (coq_Sep_simplifier sepT sepList' simpl_expr_list fuel'))))
           else (((Other
                  sepT0) :: (fst
                              (coq_Sep_simplifier sepT sepList'
                                simpl_expr_list fuel'))),
                  (snd
                    (coq_Sep_simplifier sepT sepList' simpl_expr_list fuel')))
         | None ->
           (((Other
             sepT0) :: (fst
                         (coq_Sep_simplifier sepT sepList' simpl_expr_list
                           fuel'))),
             (snd (coq_Sep_simplifier sepT sepList' simpl_expr_list fuel'))))
      | _ ->
        coq_Sep_simplifier sepT (app sepList' (sep :: [])) simpl_expr_list
          fuel'))

(** val coq_Sep_split :
    'a1 coq_Separation_def -> coq_Proposition list -> ('a1, 'a1 coq_T)
    coq_Separation list -> ('a1, 'a1 coq_T) coq_Separation list -> ident ->
    ('a1, 'a1 coq_T) coq_Separation list * ident list **)

let rec coq_Sep_split sepT propList sepList sepListRes max_id =
  match sepList with
  | [] -> ([], [])
  | sep :: sepList' ->
    (match sep with
     | Other t ->
       (match sepT.coq_Sep_split_prechecker t with
        | Some _ ->
          let (sepListSplit, eX_list) =
            sepT.coq_Sep_split_helper t propList max_id
          in
          ((app sepListSplit
             (fst (coq_Sep_split sepT propList sepList' sepListRes max_id))),
          (app eX_list
            (snd (coq_Sep_split sepT propList sepList' sepListRes max_id))))
        | None ->
          ((sep :: (fst
                     (coq_Sep_split sepT propList sepList' sepListRes max_id))),
            (snd (coq_Sep_split sepT propList sepList' sepListRes max_id))))
     | _ ->
       ((sep :: (fst (coq_Sep_split sepT propList sepList' sepListRes max_id))),
         (snd (coq_Sep_split sepT propList sepList' sepListRes max_id))))

(** val all_emp_sep :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> bool **)

let rec all_emp_sep sepT = function
| [] -> true
| h :: s' ->
  (match h with
   | Coq_emp -> all_emp_sep sepT s'
   | Time_spend _ -> all_emp_sep sepT s'
   | Other t -> sepT.is_emp t
   | _ -> false)

(** val coq_Sep_solver_with_EX :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation -> ('a1, 'a1
    coq_T) coq_Separation -> ident list -> 'a1 coq_Sep_solver_return **)

let coq_Sep_solver_with_EX sepT target resource exist_list =
  match target with
  | Coq_emp ->
    { coq_Target_sep = (target :: []); coq_Resource_sep = (resource :: []);
      coq_EX_list = exist_list; coq_Instantiation_map_list_sep = [];
      coq_Prop_list_sep = []; coq_Proofrule_list = [] }
  | Data_at (vG, t1, addressG) ->
    (match resource with
     | Data_at (vR, t2, addressR) ->
       if (&&) (eqb_val addressG addressR) (sepT.eqb_ptype t1 t2)
       then (match vG with
             | V_vari cG ->
               if coq_Find Pos.eqb exist_list cG
               then { coq_Target_sep = []; coq_Resource_sep = [];
                      coq_EX_list = exist_list;
                      coq_Instantiation_map_list_sep = ((cG, vR) :: []);
                      coq_Prop_list_sep = []; coq_Proofrule_list =
                      ((Fix_exist (cG, vR)) :: []) }
               else { coq_Target_sep = []; coq_Resource_sep = [];
                      coq_EX_list = exist_list;
                      coq_Instantiation_map_list_sep = [];
                      coq_Prop_list_sep = ((Be (Pvequal, vG, vR)) :: []);
                      coq_Proofrule_list = ((Same_cancel target) :: []) }
             | _ ->
               { coq_Target_sep = []; coq_Resource_sep = []; coq_EX_list =
                 exist_list; coq_Instantiation_map_list_sep = [];
                 coq_Prop_list_sep = ((Be (Pvequal, vG, vR)) :: []);
                 coq_Proofrule_list = ((Same_cancel target) :: []) })
       else { coq_Target_sep = (target :: []); coq_Resource_sep =
              (resource :: []); coq_EX_list = exist_list;
              coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
              coq_Proofrule_list = [] }
     | _ ->
       { coq_Target_sep = (target :: []); coq_Resource_sep =
         (resource :: []); coq_EX_list = exist_list;
         coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
         coq_Proofrule_list = [] })
  | Memory (_, _, _) ->
    (match resource with
     | Memory (_, _, _) ->
       if coq_Separation_eqb sepT target resource
       then { coq_Target_sep = []; coq_Resource_sep = []; coq_EX_list =
              exist_list; coq_Instantiation_map_list_sep = [];
              coq_Prop_list_sep = []; coq_Proofrule_list = ((Same_cancel
              target) :: []) }
       else { coq_Target_sep = (target :: []); coq_Resource_sep =
              (resource :: []); coq_EX_list = exist_list;
              coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
              coq_Proofrule_list = [] }
     | _ ->
       { coq_Target_sep = (target :: []); coq_Resource_sep =
         (resource :: []); coq_EX_list = exist_list;
         coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
         coq_Proofrule_list = [] })
  | Time_spend t1 ->
    (match resource with
     | Time_spend t2 ->
       { coq_Target_sep = []; coq_Resource_sep = []; coq_EX_list =
         exist_list; coq_Instantiation_map_list_sep = []; coq_Prop_list_sep =
         ((Be (Pvle, t2, t1)) :: []); coq_Proofrule_list = ((Time_cancel (t1,
         t2)) :: []) }
     | _ ->
       { coq_Target_sep = (target :: []); coq_Resource_sep =
         (resource :: []); coq_EX_list = exist_list;
         coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
         coq_Proofrule_list = [] })
  | Other x -> sepT.coq_Entailment_checker_with_EX x resource exist_list

(** val coq_Sep_list_solver_with_EX_helper :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1,
    'a1 coq_T) coq_Separation list -> ident list -> 'a1 coq_Sep_solver_return **)

let rec coq_Sep_list_solver_with_EX_helper sepT target resourceS exist_list =
  match resourceS with
  | [] ->
    { coq_Target_sep = target; coq_Resource_sep = resourceS; coq_EX_list =
      exist_list; coq_Instantiation_map_list_sep = []; coq_Prop_list_sep =
      []; coq_Proofrule_list = [] }
  | xR :: sR' ->
    (match target with
     | [] ->
       { coq_Target_sep = target; coq_Resource_sep = resourceS; coq_EX_list =
         exist_list; coq_Instantiation_map_list_sep = []; coq_Prop_list_sep =
         []; coq_Proofrule_list = [] }
     | xT :: sT' ->
       let s_return = coq_Sep_solver_with_EX sepT xT xR exist_list in
       (match s_return.coq_Target_sep with
        | [] ->
          { coq_Target_sep = []; coq_Resource_sep =
            (app s_return.coq_Resource_sep sR'); coq_EX_list =
            s_return.coq_EX_list; coq_Instantiation_map_list_sep =
            s_return.coq_Instantiation_map_list_sep; coq_Prop_list_sep =
            s_return.coq_Prop_list_sep; coq_Proofrule_list =
            s_return.coq_Proofrule_list }
        | xT' :: t' ->
          let s_return' =
            coq_Sep_list_solver_with_EX_helper sepT (app sT' (xT' :: t')) sR'
              s_return.coq_EX_list
          in
          { coq_Target_sep = s_return'.coq_Target_sep; coq_Resource_sep =
          (app s_return.coq_Resource_sep s_return'.coq_Resource_sep);
          coq_EX_list = s_return'.coq_EX_list;
          coq_Instantiation_map_list_sep =
          (app s_return.coq_Instantiation_map_list_sep
            s_return'.coq_Instantiation_map_list_sep); coq_Prop_list_sep =
          (app s_return.coq_Prop_list_sep s_return'.coq_Prop_list_sep);
          coq_Proofrule_list =
          (app s_return.coq_Proofrule_list s_return'.coq_Proofrule_list) }))

(** val coq_Sep_list_solver_with_EX_once :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1,
    'a1 coq_T) coq_Separation list -> ident list -> 'a1 coq_Sep_solver_return **)

let rec coq_Sep_list_solver_with_EX_once sepT targetS resourceS exist_list =
  match targetS with
  | [] ->
    { coq_Target_sep = targetS; coq_Resource_sep = resourceS; coq_EX_list =
      exist_list; coq_Instantiation_map_list_sep = []; coq_Prop_list_sep =
      []; coq_Proofrule_list = [] }
  | xG :: sG' ->
    let s_return =
      coq_Sep_list_solver_with_EX_helper sepT (xG :: []) resourceS exist_list
    in
    let s_return' =
      coq_Sep_list_solver_with_EX_once sepT sG' s_return.coq_Resource_sep
        s_return.coq_EX_list
    in
    { coq_Target_sep =
    (app s_return.coq_Target_sep s_return'.coq_Target_sep);
    coq_Resource_sep = s_return'.coq_Resource_sep; coq_EX_list =
    s_return'.coq_EX_list; coq_Instantiation_map_list_sep =
    (app s_return.coq_Instantiation_map_list_sep
      s_return'.coq_Instantiation_map_list_sep); coq_Prop_list_sep =
    (app s_return.coq_Prop_list_sep s_return'.coq_Prop_list_sep);
    coq_Proofrule_list =
    (app s_return.coq_Proofrule_list s_return'.coq_Proofrule_list) }

(** val coq_Sep_list_solver_with_EX :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ('a1,
    'a1 coq_T) coq_Separation list -> ident list -> nat -> 'a1
    coq_Sep_solver_return **)

let rec coq_Sep_list_solver_with_EX sepT targetS resourceS exist_list = function
| O ->
  { coq_Target_sep = targetS; coq_Resource_sep = resourceS; coq_EX_list =
    exist_list; coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
    coq_Proofrule_list = [] }
| S fuel' ->
  let s_return =
    coq_Sep_list_solver_with_EX_once sepT targetS resourceS exist_list
  in
  let s_return' =
    coq_Sep_list_solver_with_EX sepT s_return.coq_Target_sep
      s_return.coq_Resource_sep s_return.coq_EX_list fuel'
  in
  { coq_Target_sep = s_return'.coq_Target_sep; coq_Resource_sep =
  s_return'.coq_Resource_sep; coq_EX_list = s_return'.coq_EX_list;
  coq_Instantiation_map_list_sep =
  (app s_return.coq_Instantiation_map_list_sep
    s_return'.coq_Instantiation_map_list_sep); coq_Prop_list_sep =
  (app s_return.coq_Prop_list_sep s_return'.coq_Prop_list_sep);
  coq_Proofrule_list =
  (app s_return.coq_Proofrule_list s_return'.coq_Proofrule_list) }
