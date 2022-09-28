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

(** val coq_Add_exist_list :
    'a1 coq_Separation_def -> 'a1 assertion -> ident list -> 'a1 assertion **)

let rec coq_Add_exist_list sepT ass = function
| [] -> ass
| id :: l -> Aex (id, (coq_Add_exist_list sepT ass l))

(** val coq_Union_assert :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 assertion **)

let coq_Union_assert sepT ass =
  coq_Add_exist_list sepT (Anormal (ass.coq_Prop_list, ass.coq_Local_list,
    ass.coq_Sep_list)) ass.coq_Exist_list

(** val coq_Split_assert :
    'a1 coq_Separation_def -> 'a1 assertion -> 'a1 coq_Prod_assert **)

let rec coq_Split_assert sepT = function
| Anormal (propx, localx, sepx) ->
  { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = [] }
| Aex (id, a) ->
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert sepT a
  in
  { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
  coq_Exist_list = (id :: exist_list) }

(** val coq_Changeval_Assert :
    'a1 coq_Separation_def -> expr_val -> expr_val -> 'a1 coq_Prod_assert ->
    'a1 coq_Prod_assert **)

let coq_Changeval_Assert sepT x y a =
  match x with
  | V_vari id ->
    { coq_Prop_list = (coq_Changeval_prop_list x y a.coq_Prop_list);
      coq_Local_list = (coq_Changeval_Local x y a.coq_Local_list);
      coq_Sep_list = (coq_Changeval_sep_list sepT x y a.coq_Sep_list);
      coq_Exist_list = (coq_Remove Pos.eqb a.coq_Exist_list id) }
  | _ -> a

(** val coq_Changeval_assertion :
    'a1 coq_Separation_def -> expr_val -> expr_val -> 'a1 assertion -> 'a1
    assertion **)

let coq_Changeval_assertion sepT x y a =
  coq_Union_assert sepT
    (coq_Changeval_Assert sepT x y (coq_Split_assert sepT a))

(** val coq_Clear_Prod_error :
    'a1 coq_Separation_def -> ('a1 assertion, 'a1 coq_Prod_error) sum list ->
    'a1 assertion list **)

let rec coq_Clear_Prod_error sepT = function
| [] -> []
| s :: l' ->
  (match s with
   | Coq_inl a -> a :: (coq_Clear_Prod_error sepT l')
   | Coq_inr _ -> coq_Clear_Prod_error sepT l')

(** val clear_unuse :
    coq_Proposition list -> ident -> coq_Proposition list **)

let rec clear_unuse mP i =
  match mP with
  | [] -> []
  | p :: p0 ->
    if coq_Find_ident_p i p then clear_unuse p0 i else p :: (clear_unuse p0 i)

(** val coq_Clear_unuse :
    'a1 coq_Separation_def -> ident list -> coq_Proposition list -> coq_Local
    list -> ('a1, 'a1 coq_T) coq_Separation list -> coq_Proposition
    list * ident list **)

let rec coq_Clear_unuse sepT l mP mL mS =
  match l with
  | [] -> (mP, [])
  | a :: l' ->
    let (p, i) = coq_Clear_unuse sepT l' mP mL mS in
    if (||) (coq_Find_ident_LS sepT a mL mS) (coq_Find_ident_P a mP)
    then (p, (a :: i))
    else ((clear_unuse p a), i)

(** val coq_Clear_unuse_ass :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert **)

let coq_Clear_unuse_ass sepT ass =
  let (p, l) =
    coq_Clear_unuse sepT ass.coq_Exist_list ass.coq_Prop_list
      ass.coq_Local_list ass.coq_Sep_list
  in
  { coq_Prop_list = p; coq_Local_list = ass.coq_Local_list; coq_Sep_list =
  ass.coq_Sep_list; coq_Exist_list = l }

(** val coq_Clear_unuse_ret :
    'a1 coq_Separation_def -> 'a1 coq_Prod_ret -> 'a1 coq_Prod_ret **)

let coq_Clear_unuse_ret sepT ret =
  { coq_Assert_r = (coq_Clear_unuse_ass sepT ret.coq_Assert_r); coq_Return =
    ret.coq_Return }

(** val coq_Rename_Ass :
    'a1 coq_Separation_def -> (ident * ident) list -> 'a1 coq_Prod_assert ->
    'a1 coq_Prod_assert **)

let coq_Rename_Ass sepT change_list ass =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = ass
  in
  { coq_Prop_list = (coq_Rename_Proposition_list propx change_list);
  coq_Local_list = (coq_Rename_Local_list localx change_list); coq_Sep_list =
  (coq_Rename_Separation_list sepT sepx change_list); coq_Exist_list =
  (coq_Rename_ident exist_list change_list) }

(** val coq_Rename_ret :
    'a1 coq_Separation_def -> (ident * ident) list -> 'a1 coq_Prod_ret -> 'a1
    coq_Prod_ret **)

let coq_Rename_ret sepT change_list ret =
  { coq_Assert_r = (coq_Rename_Ass sepT change_list ret.coq_Assert_r);
    coq_Return = (coq_Rename_option ret.coq_Return change_list) }

(** val coq_Rename_instantiation_map :
    (ident * ident) list -> (ident * expr_val) list -> (ident * expr_val) list **)

let rec coq_Rename_instantiation_map change_list = function
| [] -> []
| p :: l' ->
  let (b, c) = p in
  (match look_up change_list b with
   | Some d -> (d, c)
   | None -> (b, c)) :: (coq_Rename_instantiation_map change_list l')

(** val coq_Rename_return :
    'a1 coq_Separation_def -> (ident * ident) list -> 'a1 coq_Assert_return
    -> 'a1 coq_Assert_return **)

let coq_Rename_return sepT change_list a_return =
  { coq_Target_assert =
    (coq_Rename_Ass sepT change_list a_return.coq_Target_assert);
    coq_Resource_assert =
    (coq_Rename_Ass sepT change_list a_return.coq_Resource_assert);
    instantiation_map_assert =
    (coq_Rename_instantiation_map change_list
      a_return.instantiation_map_assert); coq_Proofrule_listx =
    (coq_Rename_rule_list change_list a_return.coq_Proofrule_listx) }

(** val instantiation_pair_eqb :
    (ident * expr_val) -> (ident * expr_val) -> bool **)

let instantiation_pair_eqb p1 p2 =
  let (c1, _) = p1 in let (c2, _) = p2 in Pos.eqb c1 c2

(** val remove_duplicates :
    (ident * expr_val) list -> nat -> (ident * expr_val) list **)

let rec remove_duplicates instantiation_map fuel =
  match instantiation_map with
  | [] -> []
  | x :: l' ->
    (match fuel with
     | O -> []
     | S fuel' ->
       x :: (remove_duplicates (coq_Remove instantiation_pair_eqb l' x) fuel'))

(** val coq_Change_Val :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> (ident * expr_val) list
    -> 'a1 coq_Prod_assert **)

let rec coq_Change_Val sepT target = function
| [] -> target
| x :: l' ->
  coq_Change_Val sepT
    (coq_Split_assert sepT
      (coq_Changeval_assertion sepT (V_vari (fst x)) (snd x)
        (coq_Union_assert sepT target))) l'

(** val max_ident_in_prop : coq_Proposition -> ident **)

let rec max_ident_in_prop = function
| Up p -> max_ident_in_prop p
| Bp (_, p1, p2) -> Pos.max (max_ident_in_prop p1) (max_ident_in_prop p2)
| Ue (_, e) -> max_ident_in_expr e
| Be (_, e1, e2) -> Pos.max (max_ident_in_expr e1) (max_ident_in_expr e2)
| In_bound (low, val0, high) ->
  Pos.max (Pos.max (max_ident_in_expr low) (max_ident_in_expr val0))
    (max_ident_in_expr high)
| Qf (_, e, p) -> Pos.max (max_ident_in_expr e) (max_ident_in_prop p)
| _ -> Coq_xH

(** val max_ident_in_prop_list : coq_Proposition list -> ident **)

let rec max_ident_in_prop_list = function
| [] -> Coq_xH
| x :: l' -> Pos.max (max_ident_in_prop x) (max_ident_in_prop_list l')

(** val max_ident_in_local : coq_Local list -> ident **)

let rec max_ident_in_local = function
| [] -> Coq_xH
| x :: l' ->
  let Temp (id, v) = x in
  Pos.max (Pos.max id (max_ident_in_expr v)) (max_ident_in_local l')

(** val max_ident_in_sep :
    'a1 coq_Separation_def -> ('a1, 'a1 coq_T) coq_Separation list -> ident **)

let rec max_ident_in_sep sepT = function
| [] -> Coq_xH
| x :: l' ->
  (match x with
   | Coq_emp -> Coq_xH
   | Data_at (v, _, addr) ->
     Pos.max (Pos.max (max_ident_in_expr v) (max_ident_in_expr addr))
       (max_ident_in_sep sepT l')
   | Memory (addr, len, list) ->
     Pos.max
       (max_ident
         ((max_ident_in_expr addr) :: ((max_ident_in_expr len) :: ((max_ident_in_expr
                                                                    list) :: []))))
       (max_ident_in_sep sepT l')
   | Time_spend t1 -> max_ident_in_expr t1
   | Other x0 -> sepT.max_ident_T x0)

(** val max_ident_in_assertion :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> ident **)

let max_ident_in_assertion sepT resource =
  let { coq_Prop_list = propxG; coq_Local_list = localxG; coq_Sep_list =
    sepxG; coq_Exist_list = _ } = resource
  in
  max_ident
    ((max_ident_in_prop_list propxG) :: ((max_ident_in_local localxG) :: (
    (max_ident_in_sep sepT sepxG) :: [])))

(** val coq_Ass_unification :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert ->
    'a1 coq_Prod_assert **)

let coq_Ass_unification sepT target resource =
  let init_max_ident = max_ident_in_assertion sepT resource in
  coq_Rename_Ass sepT
    (coq_Add_ident_map target.coq_Exist_list init_max_ident) target

(** val coq_Return_back :
    'a1 coq_Separation_def -> 'a1 coq_Assert_return -> 'a1 coq_Prod_assert ->
    'a1 coq_Prod_assert -> 'a1 coq_Assert_return **)

let coq_Return_back sepT a_return target resource =
  let change_list =
    coq_Rev_ident_list
      (coq_Add_ident_map target.coq_Exist_list
        (max_ident_in_assertion sepT resource))
  in
  coq_Rename_return sepT change_list a_return
