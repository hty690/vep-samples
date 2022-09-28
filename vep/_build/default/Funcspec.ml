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

(** val coq_Change_Val_option :
    'a1 coq_Separation_def -> (ident * expr_val) list -> expr_val option ->
    expr_val option **)

let rec coq_Change_Val_option sepT l a =
  match l with
  | [] -> a
  | p :: l' ->
    let (x, y) = p in
    coq_Change_Val_option sepT l' (coq_Changeval_option (V_vari x) y a)

(** val list_Changeval_option :
    'a1 coq_Separation_def -> (expr_val * expr_val) list -> expr_val option
    -> expr_val option **)

let rec list_Changeval_option sepT l a =
  match l with
  | [] -> a
  | p :: l' ->
    let (x, y) = p in
    list_Changeval_option sepT l' (coq_Changeval_option x y a)

(** val coq_Changeval_list_ret :
    'a1 coq_Separation_def -> (ident * expr_val) list -> 'a1 coq_Prod_ret ->
    'a1 coq_Prod_ret **)

let coq_Changeval_list_ret sepT l ret =
  { coq_Assert_r = (coq_Change_Val sepT ret.coq_Assert_r l); coq_Return =
    (coq_Change_Val_option sepT l ret.coq_Return) }

(** val coq_Look_val :
    'a1 coq_Separation_def -> ident list -> 'a1 coq_Prod_assert -> expr_val
    list **)

let coq_Look_val _ arg a =
  coq_Clear_option (map (eval_tmpval a.coq_Local_list) arg)

(** val list_Changeval :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> (expr_val * expr_val)
    list -> 'a1 coq_Prod_assert **)

let rec list_Changeval sepT a = function
| [] -> a
| p :: l' ->
  let (a0, b) = p in coq_Changeval_Assert sepT a0 b (list_Changeval sepT a l')

(** val list_Changeval_ret :
    'a1 coq_Separation_def -> (expr_val * expr_val) list -> 'a1 coq_Prod_ret
    -> 'a1 coq_Prod_ret **)

let list_Changeval_ret sepT l ret =
  { coq_Assert_r = (list_Changeval sepT ret.coq_Assert_r l); coq_Return =
    (list_Changeval_option sepT l ret.coq_Return) }

(** val coq_Substitution_func :
    'a1 coq_Separation_def -> 'a1 funcdes -> expr_val list -> 'a1
    coq_Prod_assert * 'a1 coq_Prod_ret **)

let coq_Substitution_func sepT f args =
  let change_list1 = combine (coq_Look_val sepT f.coq_Args f.coq_Precon) args
  in
  let change_list2 =
    combine (coq_Look_val sepT f.coq_Args f.coq_Postcon.coq_Assert_r) args
  in
  ((coq_Clear_unuse_ass sepT (list_Changeval sepT f.coq_Precon change_list1)),
  (coq_Clear_unuse_ret sepT
    (list_Changeval_ret sepT change_list2 f.coq_Postcon)))

(** val coq_New_name_func :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 funcdes -> 'a1
    funcdes **)

let coq_New_name_func sepT p f =
  let change_list =
    coq_Add_ident_map (app f.coq_Param f.coq_Precon.coq_Exist_list)
      (max_ident_in_assertion sepT p)
  in
  { coq_Args = f.coq_Args; coq_Param =
  (coq_Rename_ident f.coq_Param change_list); coq_Precon =
  (coq_Rename_Ass sepT change_list f.coq_Precon); coq_Postcon =
  (coq_Rename_ret sepT change_list f.coq_Postcon) }

(** val coq_Merge_Assert :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert ->
    'a1 coq_Prod_assert **)

let coq_Merge_Assert _ p1 p2 =
  { coq_Prop_list = (app p1.coq_Prop_list p2.coq_Prop_list); coq_Local_list =
    (app p1.coq_Local_list p2.coq_Local_list); coq_Sep_list =
    (app p1.coq_Sep_list p2.coq_Sep_list); coq_Exist_list =
    (app p1.coq_Exist_list p2.coq_Exist_list) }

(** val coq_Clear_Local :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 coq_Prod_assert **)

let coq_Clear_Local _ p =
  { coq_Prop_list = p.coq_Prop_list; coq_Local_list = []; coq_Sep_list =
    p.coq_Sep_list; coq_Exist_list = p.coq_Exist_list }

(** val coq_Fill_logical_list :
    'a1 coq_Separation_def -> 'a1 coq_Prod_ret -> ident list ->
    (ident * expr_val) list -> 'a1 coq_Prod_ret option **)

let rec coq_Fill_logical_list sepT p l instantiation_map =
  match l with
  | [] ->
    Some { coq_Assert_r = (coq_Clear_Local sepT p.coq_Assert_r); coq_Return =
      p.coq_Return }
  | a :: l' ->
    (match coq_Find_A_in_prodAB Pos.eqb instantiation_map a with
     | Some v ->
       coq_Fill_logical_list sepT
         (coq_Changeval_list_ret sepT ((a, v) :: []) p) l' instantiation_map
     | None -> None)

(** val coq_Change_for_func :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 funcdes -> expr_val
    option list -> 'a1 coq_Prod_ret **)

let coq_Change_for_func sepT p f arg =
  if (&&) (negb (coq_Find_None arg))
       (Nat.eqb (length arg) (length f.coq_Args))
  then let f' = coq_New_name_func sepT p f in
       let (pre', post') =
         coq_Substitution_func sepT f' (coq_Clear_option arg)
       in
       let pre = { coq_Prop_list = pre'.coq_Prop_list; coq_Local_list = [];
         coq_Sep_list = pre'.coq_Sep_list; coq_Exist_list =
         (app pre'.coq_Exist_list f'.coq_Param) }
       in
       let a_return =
         coq_Ass_solver sepT (coq_Union_assert sepT pre)
           (coq_Union_assert sepT p)
       in
       (match coq_Fill_logical_list sepT post' f'.coq_Param
                a_return.instantiation_map_assert with
        | Some post ->
          { coq_Assert_r =
            (coq_Merge_Assert sepT a_return.coq_Resource_assert
              post.coq_Assert_r); coq_Return = post.coq_Return }
        | None -> { coq_Assert_r = p; coq_Return = None })
  else { coq_Assert_r = p; coq_Return = None }

(** val coq_Suitable :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 funcdes -> expr_val
    option list -> bool **)

let coq_Suitable sepT p f arg =
  if (&&) (negb (coq_Find_None arg))
       (Nat.eqb (length arg) (length f.coq_Args))
  then let f' = coq_New_name_func sepT p f in
       let (pre', _) = coq_Substitution_func sepT f' (coq_Clear_option arg) in
       let pre = { coq_Prop_list = pre'.coq_Prop_list; coq_Local_list = [];
         coq_Sep_list = pre'.coq_Sep_list; coq_Exist_list =
         (app pre'.coq_Exist_list f'.coq_Param) }
       in
       coq_Partial_Ass_solver_bool sepT (coq_Union_assert sepT p)
         (coq_Union_assert sepT pre)
  else false

(** val coq_Change_for_func_list :
    'a1 coq_Separation_def -> 'a1 coq_Prod_assert -> 'a1 funcdes list ->
    expr_val option list -> 'a1 coq_Prod_ret list **)

let rec coq_Change_for_func_list sepT p l arg =
  match l with
  | [] -> []
  | a :: l' ->
    if coq_Suitable sepT p a arg
    then (coq_Change_for_func sepT p a arg) :: (coq_Change_for_func_list sepT
                                                 p l' arg)
    else coq_Change_for_func_list sepT p l' arg
