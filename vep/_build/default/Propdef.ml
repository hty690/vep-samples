open BinNums
open Datatypes
open Exprdef
open List_lemma

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val coq_Up_eqb : __ -> bool **)

let coq_Up_eqb _ =
  true

type coq_Binary_prop_op =
| Por
| Pand
| Pimply
| Piff

(** val coq_Bp_eqb : coq_Binary_prop_op -> coq_Binary_prop_op -> bool **)

let coq_Bp_eqb x y =
  match x with
  | Por -> (match y with
            | Por -> true
            | _ -> false)
  | Pand -> (match y with
             | Pand -> true
             | _ -> false)
  | Pimply -> (match y with
               | Pimply -> true
               | _ -> false)
  | Piff -> (match y with
             | Piff -> true
             | _ -> false)

type coq_Unary_expr_op =
| Pisptr
| Pis_pointer_or_null

(** val coq_Ue_eqb : coq_Unary_expr_op -> coq_Unary_expr_op -> bool **)

let coq_Ue_eqb x y =
  match x with
  | Pisptr -> (match y with
               | Pisptr -> true
               | Pis_pointer_or_null -> false)
  | Pis_pointer_or_null ->
    (match y with
     | Pisptr -> false
     | Pis_pointer_or_null -> true)

type coq_Binary_expr_op =
| Pvle
| Pvge
| Pvlt
| Pvgt
| Pvequal
| Psvle
| Psvge
| Psvlt
| Psvgt
| Share

(** val coq_Get_not_be_op :
    coq_Binary_expr_op -> coq_Binary_expr_op option **)

let coq_Get_not_be_op = function
| Pvle -> Some Pvgt
| Pvge -> Some Pvlt
| Pvlt -> Some Pvge
| Pvgt -> Some Pvle
| Psvle -> Some Psvgt
| Psvge -> Some Psvlt
| Psvlt -> Some Psvge
| Psvgt -> Some Psvle
| _ -> None

(** val coq_Get_neg_be_op :
    coq_Binary_expr_op -> coq_Binary_expr_op option **)

let coq_Get_neg_be_op = function
| Pvle -> Some Pvge
| Pvge -> Some Pvle
| Pvlt -> Some Pvgt
| Pvgt -> Some Pvlt
| Psvle -> Some Psvge
| Psvge -> Some Psvle
| Psvlt -> Some Psvgt
| Psvgt -> Some Psvlt
| _ -> None

(** val cancel_op : coq_Binary_expr_op -> bool **)

let cancel_op = function
| Share -> false
| _ -> true

(** val coq_Be_eqb : coq_Binary_expr_op -> coq_Binary_expr_op -> bool **)

let coq_Be_eqb x y =
  match x with
  | Pvle -> (match y with
             | Pvle -> true
             | _ -> false)
  | Pvge -> (match y with
             | Pvge -> true
             | _ -> false)
  | Pvlt -> (match y with
             | Pvlt -> true
             | _ -> false)
  | Pvgt -> (match y with
             | Pvgt -> true
             | _ -> false)
  | Pvequal -> (match y with
                | Pvequal -> true
                | _ -> false)
  | Psvle -> (match y with
              | Psvle -> true
              | _ -> false)
  | Psvge -> (match y with
              | Psvge -> true
              | _ -> false)
  | Psvlt -> (match y with
              | Psvlt -> true
              | _ -> false)
  | Psvgt -> (match y with
              | Psvgt -> true
              | _ -> false)
  | Share -> (match y with
              | Share -> true
              | _ -> false)

type coq_Quantifier =
| PForall
| PExists

(** val coq_Quantifier_eqb : coq_Quantifier -> coq_Quantifier -> bool **)

let coq_Quantifier_eqb x y =
  match x with
  | PForall -> (match y with
                | PForall -> true
                | PExists -> false)
  | PExists -> (match y with
                | PForall -> false
                | PExists -> true)

type coq_Proposition =
| TT
| FF
| Bot
| Up of coq_Proposition
| Bp of coq_Binary_prop_op * coq_Proposition * coq_Proposition
| Ue of coq_Unary_expr_op * expr_val
| Be of coq_Binary_expr_op * expr_val * expr_val
| In_bound of expr_val * expr_val * expr_val
| Qf of coq_Quantifier * expr_val * coq_Proposition

(** val coq_Proposition_eqb : coq_Proposition -> coq_Proposition -> bool **)

let rec coq_Proposition_eqb p1 p2 =
  match p1 with
  | TT -> (match p2 with
           | TT -> true
           | _ -> false)
  | FF -> (match p2 with
           | FF -> true
           | _ -> false)
  | Bot -> (match p2 with
            | Bot -> true
            | _ -> false)
  | Up p ->
    (match p2 with
     | Up p' -> (&&) (coq_Up_eqb __) (coq_Proposition_eqb p p')
     | _ -> false)
  | Bp (op, p, p3) ->
    (match p2 with
     | Bp (op', p', p1') ->
       (&&) ((&&) (coq_Bp_eqb op op') (coq_Proposition_eqb p p'))
         (coq_Proposition_eqb p3 p1')
     | _ -> false)
  | Ue (op, e) ->
    (match p2 with
     | Ue (op', e') -> (&&) (coq_Ue_eqb op op') (eqb_val e e')
     | _ -> false)
  | Be (op, e, e1) ->
    (match p2 with
     | Be (op', e', e1') ->
       (&&) ((&&) (coq_Be_eqb op op') (eqb_val e e')) (eqb_val e1 e1')
     | _ -> false)
  | In_bound (low, val0, high) ->
    (match p2 with
     | In_bound (low', val', high') ->
       (&&) ((&&) (eqb_val low low') (eqb_val val0 val')) (eqb_val high high')
     | _ -> false)
  | Qf (op, e, p) ->
    (match p2 with
     | Qf (op', e', p') ->
       (&&) ((&&) (coq_Quantifier_eqb op op') (eqb_val e e'))
         (coq_Proposition_eqb p p')
     | _ -> false)

(** val coq_Find_Prop_in_list :
    coq_Proposition list -> coq_Proposition -> bool **)

let coq_Find_Prop_in_list p p0 =
  coq_Find coq_Proposition_eqb p p0

(** val coq_Simple_up_op : coq_Proposition -> coq_Proposition **)

let rec coq_Simple_up_op p = match p with
| TT -> FF
| FF -> TT
| Up p' -> p'
| Bp (op', p1, p2) ->
  (match op' with
   | Por -> Bp (Pand, (coq_Simple_up_op p1), (coq_Simple_up_op p2))
   | Pand -> Bp (Por, (coq_Simple_up_op p1), (coq_Simple_up_op p2))
   | Pimply -> Bp (Pand, p1, (coq_Simple_up_op p2))
   | Piff ->
     Bp (Por, (Bp (Pand, p1, (coq_Simple_up_op p2))), (Bp (Pand, p2,
       (coq_Simple_up_op p1)))))
| Be (op', e1, e2) ->
  (match coq_Get_not_be_op op' with
   | Some op1 -> Be (op1, e1, e2)
   | None -> Up p)
| _ -> Up p

(** val coq_Simple_be_op :
    coq_Binary_expr_op -> expr_val -> expr_val -> coq_Proposition **)

let coq_Simple_be_op op e1 e2 =
  match e1 with
  | V_vari _ -> Be (op, e1, e2)
  | Vlist_vari _ -> Be (op, e1, e2)
  | _ ->
    (match e2 with
     | V_vari _ -> Be (op, e1, e2)
     | Vlist_vari _ -> Be (op, e1, e2)
     | _ ->
       if cancel_op op
       then let s1 = coq_Transfer e1 in
            let s2 = coq_Transfer e2 in
            let same = coq_Same_part formal_part_eqb s1 s2 in
            Be (op,
            (coq_Retransfer (coq_Remove_part formal_part_eqb s1 same)),
            (coq_Retransfer (coq_Remove_part formal_part_eqb s2 same)))
       else Be (op, e1, e2))

(** val coq_Simple_ue_op :
    coq_Unary_expr_op -> expr_val -> coq_Proposition **)

let coq_Simple_ue_op op e =
  match op with
  | Pisptr -> if eqb_val e (Ez_val Z0) then FF else Ue (op, e)
  | Pis_pointer_or_null -> if eqb_val e (Ez_val Z0) then TT else Ue (op, e)

(** val coq_Main_bounded : expr_val -> expr_val **)

let coq_Main_bounded v = match v with
| Vlop (l, n, _) -> (match l with
                     | Vnth -> n
                     | _ -> v)
| _ -> v

(** val coq_Simplify_prop : coq_Proposition -> coq_Proposition **)

let rec coq_Simplify_prop s = match s with
| Up p -> coq_Simple_up_op (coq_Simplify_prop p)
| Bp (op, p1, p2) -> Bp (op, (coq_Simplify_prop p1), (coq_Simplify_prop p2))
| Ue (op, e) -> coq_Simple_ue_op op (coq_Simplify_val e)
| Be (op, e1, e2) ->
  coq_Simple_be_op op (coq_Simplify_val e1) (coq_Simplify_val e2)
| In_bound (low, val0, high) ->
  In_bound ((coq_Simplify_val low), (coq_Simplify_val val0),
    (coq_Simplify_val high))
| Qf (op, e, p) -> Qf (op, (coq_Simplify_val e), (coq_Simplify_prop p))
| _ -> s

(** val simpl_Proposition_list_1 :
    coq_Proposition list -> coq_Proposition list **)

let rec simpl_Proposition_list_1 = function
| [] -> []
| p :: l0 ->
  let l' = simpl_Proposition_list_1 l0 in
  let p' = coq_Simplify_prop p in
  if coq_Find coq_Proposition_eqb l' p' then l' else p' :: l'

(** val coq_Split_and_iff_bound_one :
    coq_Proposition -> coq_Proposition list **)

let rec coq_Split_and_iff_bound_one s = match s with
| Bp (op, p1, p2) ->
  (match op with
   | Pand ->
     app (coq_Split_and_iff_bound_one p1) (coq_Split_and_iff_bound_one p2)
   | Piff -> (Bp (Pimply, p1, p2)) :: ((Bp (Pimply, p2, p1)) :: [])
   | _ -> s :: [])
| In_bound (low, val0, high) ->
  (Be (Pvle, low, (coq_Main_bounded val0))) :: ((Be (Pvle,
    (coq_Main_bounded val0), high)) :: [])
| _ -> s :: []

(** val coq_Split_and_iff_bound :
    coq_Proposition list -> coq_Proposition list **)

let rec coq_Split_and_iff_bound = function
| [] -> []
| p :: l0 ->
  let l' = coq_Split_and_iff_bound l0 in
  let p' = coq_Split_and_iff_bound_one p in
  let same = coq_Same_part coq_Proposition_eqb l' p' in
  app same
    (app (coq_Remove_part coq_Proposition_eqb l' same)
      (coq_Remove_part coq_Proposition_eqb p' same))

(** val simpl_Proposition_list :
    coq_Proposition list -> coq_Proposition list **)

let simpl_Proposition_list l =
  coq_Split_and_iff_bound (simpl_Proposition_list_1 l)

(** val coq_Changeval_prop :
    expr_val -> expr_val -> coq_Proposition -> coq_Proposition **)

let rec coq_Changeval_prop x y s = match s with
| Up p -> Up (coq_Changeval_prop x y p)
| Bp (op, p1, p2) ->
  Bp (op, (coq_Changeval_prop x y p1), (coq_Changeval_prop x y p2))
| Ue (op, e) -> Ue (op, (coq_Changeval x y e))
| Be (op, e1, e2) -> Be (op, (coq_Changeval x y e1), (coq_Changeval x y e2))
| In_bound (low, val0, high) ->
  In_bound ((coq_Changeval x y low), (coq_Changeval x y val0),
    (coq_Changeval x y high))
| Qf (op, e, p) -> Qf (op, (coq_Changeval x y e), (coq_Changeval_prop x y p))
| _ -> s

(** val coq_Changeval_prop_list :
    expr_val -> expr_val -> coq_Proposition list -> coq_Proposition list **)

let rec coq_Changeval_prop_list x y = function
| [] -> []
| p :: s' -> (coq_Changeval_prop x y p) :: (coq_Changeval_prop_list x y s')

(** val coq_Find_prop_in_used :
    expr_val -> coq_Proposition -> coq_Proposition option **)

let rec coq_Find_prop_in_used x p = match p with
| Up p0 ->
  (match coq_Find_prop_in_used x p0 with
   | Some _ -> Some p
   | None -> None)
| Bp (_, p1, p2) ->
  (match coq_Find_prop_in_used x p1 with
   | Some _ -> Some p
   | None ->
     (match coq_Find_prop_in_used x p2 with
      | Some _ -> Some p
      | None -> None))
| Ue (_, e) -> if eqb_val e x then Some p else None
| Be (_, e1, e2) ->
  if (||) (eqb_val x e1) (eqb_val x e2) then Some p else None
| In_bound (low, val0, high) ->
  if (||) ((||) (eqb_val x low) (eqb_val x val0)) (eqb_val x high)
  then Some p
  else None
| Qf (_, e, p0) ->
  if eqb_val e x
  then Some p
  else (match coq_Find_prop_in_used x p0 with
        | Some _ -> Some p
        | None -> None)
| _ -> None

(** val coq_Find_prop_in_used_list :
    expr_val -> coq_Proposition list -> coq_Proposition option **)

let rec coq_Find_prop_in_used_list x = function
| [] -> None
| p :: s' ->
  (match coq_Find_prop_in_used x p with
   | Some c -> Some c
   | None -> coq_Find_prop_in_used_list x s')

(** val coq_Change_prop_vequal :
    coq_Proposition list -> expr_val -> expr_val -> coq_Proposition list **)

let coq_Change_prop_vequal propx x y =
  match coq_Find_prop_in_used_list x propx with
  | Some p ->
    (match p with
     | Be (b, x1, y1) ->
       (match b with
        | Pvequal ->
          (Be (Pvequal, x,
            y)) :: (coq_Remove coq_Proposition_eqb propx (Be (Pvequal, x1,
                     y1)))
        | _ -> (Be (Pvequal, x, y)) :: propx)
     | _ -> (Be (Pvequal, x, y)) :: propx)
  | None -> (Be (Pvequal, x, y)) :: propx
