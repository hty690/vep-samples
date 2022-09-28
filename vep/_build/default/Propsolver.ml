open BinInt
open BinNums
open Datatypes
open Nat
open Propdef
open Exprdef
open List_lemma

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val option_not : bool option -> bool option **)

let option_not = function
| Some b -> if b then Some false else Some true
| None -> None

(** val option_and : bool option -> bool option -> bool option **)

let option_and a b =
  match a with
  | Some a' -> (match b with
                | Some b' -> Some ((&&) a' b')
                | None -> None)
  | None -> None

(** val option_or : bool option -> bool option -> bool option **)

let option_or a b =
  match a with
  | Some a' -> (match b with
                | Some b' -> Some ((||) a' b')
                | None -> a)
  | None -> b

(** val option_merge :
    (bool * expr_val option) -> (bool * expr_val option) -> bool * expr_val
    option **)

let option_merge a b =
  if (&&) (fst a) (fst b)
  then (match snd a with
        | Some e1 ->
          (match snd b with
           | Some e2 ->
             if eqb_val e1 e2 then (true, (Some e1)) else (false, None)
           | None -> (true, (snd a)))
        | None -> (true, (snd b)))
  else (false, None)

(** val coq_Check_for_vle : expr_val -> expr_val -> bool option **)

let coq_Check_for_vle a b =
  match a with
  | Ez_val a' ->
    (match b with
     | Ez_val b' -> Some (Z.leb a' b')
     | _ -> if eqb_val a b then Some true else None)
  | _ -> if eqb_val a b then Some true else None

(** val coq_Check_for_vlt : expr_val -> expr_val -> bool option **)

let coq_Check_for_vlt a b =
  match a with
  | Ez_val a' ->
    (match b with
     | Ez_val b' -> Some (Z.ltb a' b')
     | _ -> if eqb_val a b then Some false else None)
  | _ -> if eqb_val a b then Some false else None

(** val coq_Pvle_solve :
    expr_val -> expr_val -> coq_Proposition list -> nat -> bool option **)

let rec coq_Pvle_solve a b resource = function
| O -> coq_Check_for_vle a b
| S n0 ->
  (match coq_Find_prop_in_used_list a resource with
   | Some c ->
     let np = remove_once coq_Proposition_eqb resource c in
     (match c with
      | Up p ->
        (match p with
         | Be (b0, a1, b1) ->
           (match b0 with
            | Pvequal ->
              if (||)
                   (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal,
                     a, b)))
                   (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal,
                     b, a)))
              then coq_Pvlt_solve a b np n0
              else coq_Pvle_solve a b np n0
            | _ -> coq_Pvle_solve a b np n0)
         | _ -> coq_Pvle_solve a b np n0)
      | Be (b0, a1, b1) ->
        (match b0 with
         | Pvle ->
           if eqb_val a1 a
           then option_or (coq_Pvle_solve b1 b np n0)
                  (coq_Pvle_solve a b np n0)
           else coq_Pvle_solve a b np n0
         | Pvge ->
           if eqb_val b1 a
           then option_or (coq_Pvle_solve a1 b np n0)
                  (coq_Pvle_solve a b np n0)
           else coq_Pvle_solve a b np n0
         | Pvlt ->
           if eqb_val a1 a
           then option_or (coq_Pvle_solve b1 b np n0)
                  (coq_Pvle_solve a b np n0)
           else if (&&) (eqb_val a1 b) (eqb_val b1 a)
                then Some false
                else coq_Pvle_solve a b np n0
         | Pvgt ->
           if eqb_val b1 a
           then option_or (coq_Pvle_solve a1 b np n0)
                  (coq_Pvle_solve a b np n0)
           else if (&&) (eqb_val b1 b) (eqb_val b1 a)
                then Some false
                else coq_Pvle_solve a b np n0
         | Pvequal ->
           option_or
             (coq_Pvle_solve (if eqb_val a1 a then b1 else a1) b np n0)
             (coq_Pvle_solve a b np n0)
         | _ -> coq_Pvle_solve a b np n0)
      | _ -> coq_Pvle_solve a b np n0)
   | None -> coq_Check_for_vle a b)

(** val coq_Pvlt_solve :
    expr_val -> expr_val -> coq_Proposition list -> nat -> bool option **)

and coq_Pvlt_solve a b resource = function
| O -> coq_Check_for_vlt a b
| S n0 ->
  (match coq_Find_prop_in_used_list a resource with
   | Some c ->
     let np = remove_once coq_Proposition_eqb resource c in
     (match c with
      | Be (b0, a1, b1) ->
        (match b0 with
         | Pvle ->
           if eqb_val a1 a
           then option_or (coq_Pvlt_solve b1 b np n0)
                  (coq_Pvlt_solve a b np n0)
           else if (&&) (eqb_val a1 b) (eqb_val b1 a)
                then Some false
                else coq_Pvlt_solve a b np n0
         | Pvge ->
           if eqb_val b1 a
           then option_or (coq_Pvlt_solve a1 b np n0)
                  (coq_Pvlt_solve a b np n0)
           else if (&&) (eqb_val b1 b) (eqb_val b1 a)
                then Some false
                else coq_Pvlt_solve a b np n0
         | Pvlt ->
           if eqb_val a1 a
           then option_or (coq_Pvle_solve b1 b np n0)
                  (coq_Pvlt_solve a b np n0)
           else if (&&) (eqb_val a1 b) (eqb_val b1 a)
                then Some false
                else coq_Pvlt_solve a b np n0
         | Pvgt ->
           if eqb_val b1 a
           then option_or (coq_Pvle_solve a1 b np n0)
                  (coq_Pvlt_solve a b np n0)
           else if (&&) (eqb_val b1 b) (eqb_val b1 a)
                then Some false
                else coq_Pvlt_solve a b np n0
         | Pvequal ->
           option_or
             (coq_Pvlt_solve (if eqb_val a1 a then b1 else a1) b np n0)
             (coq_Pvlt_solve a b np n0)
         | _ -> coq_Pvlt_solve a b np n0)
      | _ -> coq_Pvlt_solve a b np n0)
   | None -> coq_Check_for_vlt a b)

(** val coq_Pvge_solve :
    expr_val -> expr_val -> coq_Proposition list -> nat -> bool option **)

let coq_Pvge_solve a b resource n =
  coq_Pvle_solve b a resource n

(** val coq_Pvgt_solve :
    expr_val -> expr_val -> coq_Proposition list -> nat -> bool option **)

let coq_Pvgt_solve a b resource n =
  coq_Pvlt_solve b a resource n

(** val coq_Pvequal_solve :
    expr_val -> expr_val -> coq_Proposition list -> nat -> bool option **)

let rec coq_Pvequal_solve a b resource = function
| O -> if eqb_val a b then Some true else None
| S n0 ->
  (match coq_Find_prop_in_used_list a resource with
   | Some c ->
     let np = remove_once coq_Proposition_eqb resource c in
     (match c with
      | Up p ->
        (match p with
         | Be (b0, a1, b1) ->
           (match b0 with
            | Pvequal ->
              if (||)
                   (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal,
                     a, b)))
                   (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal,
                     b, a)))
              then Some false
              else coq_Pvequal_solve a b np n0
            | _ -> coq_Pvequal_solve a b np n0)
         | _ -> coq_Pvequal_solve a b np n0)
      | Be (b0, a1, b1) ->
        (match b0 with
         | Pvle ->
           if (||)
                (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal, a,
                  b)))
                (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal, b,
                  a)))
           then option_or (coq_Pvle_solve b1 a1 np n0)
                  (coq_Pvequal_solve a b np n0)
           else coq_Pvequal_solve a b np n0
         | Pvge ->
           if (||)
                (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal, a,
                  b)))
                (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal, b,
                  a)))
           then option_or (coq_Pvge_solve b1 a1 np n0)
                  (coq_Pvequal_solve a b np n0)
           else coq_Pvequal_solve a b np n0
         | Pvlt ->
           if (||)
                (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal, a,
                  b)))
                (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal, b,
                  a)))
           then Some false
           else coq_Pvequal_solve a b np n0
         | Pvgt ->
           if (||)
                (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal, a,
                  b)))
                (coq_Proposition_eqb (Be (Pvequal, a1, b1)) (Be (Pvequal, b,
                  a)))
           then Some false
           else coq_Pvequal_solve a b np n0
         | Pvequal ->
           option_or
             (coq_Pvequal_solve (if eqb_val a1 a then b1 else a1) b np n0)
             (coq_Pvequal_solve a b np n0)
         | _ -> coq_Pvequal_solve a b np n0)
      | _ -> coq_Pvequal_solve a b np n0)
   | None -> if eqb_val a b then Some true else None)

(** val coq_Pis_pointer_or_null_solve :
    expr_val -> coq_Proposition list -> nat -> bool option **)

let rec coq_Pis_pointer_or_null_solve a resource = function
| O -> None
| S n0 ->
  (match coq_Find_prop_in_used_list a resource with
   | Some c ->
     let np = remove_once coq_Proposition_eqb resource c in
     (match c with
      | Up p ->
        (match p with
         | Ue (u0, _) ->
           (match u0 with
            | Pisptr -> coq_Pis_pointer_or_null_solve a np n0
            | Pis_pointer_or_null -> Some false)
         | _ -> coq_Pis_pointer_or_null_solve a np n0)
      | Ue (_, _) -> Some true
      | Be (b, a', b') ->
        (match b with
         | Pvequal ->
           option_or
             (coq_Pis_pointer_or_null_solve (if eqb_val a' a then b' else a')
               np n0) (coq_Pis_pointer_or_null_solve a np n0)
         | _ -> coq_Pis_pointer_or_null_solve a np n0)
      | _ -> coq_Pis_pointer_or_null_solve a np n0)
   | None -> None)

(** val coq_Pisptr_solve :
    expr_val -> coq_Proposition list -> nat -> bool option **)

let rec coq_Pisptr_solve a resource = function
| O -> None
| S n0 ->
  (match coq_Find_prop_in_used_list a resource with
   | Some c ->
     let np = remove_once coq_Proposition_eqb resource c in
     (match c with
      | Up p ->
        (match p with
         | Ue (_, _) -> Some false
         | Be (b, a', b') ->
           (match b with
            | Pvequal ->
              if (||)
                   (coq_Proposition_eqb (Be (Pvequal, a', b')) (Be (Pvequal,
                     a, nullptr)))
                   (coq_Proposition_eqb (Be (Pvequal, a', b')) (Be (Pvequal,
                     nullptr, a)))
              then Some true
              else coq_Pisptr_solve a np n0
            | _ -> coq_Pisptr_solve a np n0)
         | _ -> coq_Pisptr_solve a np n0)
      | Ue (u, _) ->
        (match u with
         | Pisptr -> Some true
         | Pis_pointer_or_null -> coq_Pisptr_solve a np n0)
      | Be (b, a', b') ->
        (match b with
         | Pvequal ->
           option_or
             (coq_Pisptr_solve (if eqb_val a' a then b' else a') np n0)
             (coq_Pisptr_solve a np n0)
         | _ -> coq_Pisptr_solve a np n0)
      | _ -> coq_Pisptr_solve a np n0)
   | None -> None)

(** val coq_Similar_ignore_i_val :
    expr_val -> expr_val -> expr_val -> bool * expr_val option **)

let coq_Similar_ignore_i_val _ _ _ =
  (false, None)

(** val coq_Similar_ignore_i_prop :
    expr_val -> coq_Proposition -> coq_Proposition -> bool * expr_val option **)

let rec coq_Similar_ignore_i_prop i p1 p2 =
  match p1 with
  | TT -> (match p2 with
           | TT -> (true, None)
           | _ -> (false, None))
  | Bot -> (match p2 with
            | Bot -> (true, None)
            | _ -> (false, None))
  | Up p ->
    (match p2 with
     | Up p' ->
       if coq_Up_eqb __
       then coq_Similar_ignore_i_prop i p p'
       else (false, None)
     | _ -> (false, None))
  | Bp (op, p, p3) ->
    (match p2 with
     | Bp (op', p', p1') ->
       if coq_Bp_eqb op op'
       then option_merge (coq_Similar_ignore_i_prop i p p')
              (coq_Similar_ignore_i_prop i p3 p1')
       else (false, None)
     | _ -> (false, None))
  | Ue (op, e) ->
    (match p2 with
     | Ue (op', e') ->
       if coq_Ue_eqb op op'
       then coq_Similar_ignore_i_val i e e'
       else (false, None)
     | _ -> (false, None))
  | Be (op, e, e1) ->
    (match p2 with
     | Be (_, e', e1') ->
       if coq_Be_eqb op op
       then option_merge (coq_Similar_ignore_i_val i e e')
              (coq_Similar_ignore_i_val i e1 e1')
       else (false, None)
     | _ -> (false, None))
  | In_bound (low, val0, high) ->
    (match p2 with
     | In_bound (low', val', high') ->
       option_merge
         (option_merge (coq_Similar_ignore_i_val i low low')
           (coq_Similar_ignore_i_val i val0 val'))
         (coq_Similar_ignore_i_val i high high')
     | _ -> (false, None))
  | _ -> (false, None)

(** val coq_Try_solve :
    coq_Proposition -> coq_Proposition -> bool * coq_Proposition option **)

let coq_Try_solve target resource =
  if coq_Proposition_eqb target resource
  then (true, None)
  else (match resource with
        | Up p ->
          if coq_Proposition_eqb target p
          then (false, None)
          else (false, (Some target))
        | Bp (op, p1, p2) ->
          (match op with
           | Pimply ->
             if coq_Proposition_eqb target p2
             then (false, (Some p1))
             else (false, (Some target))
           | _ -> (false, (Some target)))
        | Be (op, e1, e2) ->
          (match op with
           | Pvequal ->
             (match coq_Find_prop_in_used e1 target with
              | Some _ -> (false, (Some (coq_Changeval_prop e1 e2 target)))
              | None -> (false, (Some (coq_Changeval_prop e2 e1 target))))
           | _ -> (false, (Some target)))
        | Qf (_, e, p) ->
          (match p with
           | Bp (b, p1, p2) ->
             (match b with
              | Pimply ->
                let (b0, o) = coq_Similar_ignore_i_prop e p2 target in
                if b0
                then (match o with
                      | Some e' ->
                        (false, (Some (coq_Changeval_prop e e' p1)))
                      | None -> (true, None))
                else (false, (Some target))
              | _ -> (false, (Some target)))
           | _ -> (false, (Some target)))
        | _ -> (false, (Some target)))

(** val coq_Search_for_related :
    coq_Proposition -> coq_Proposition list -> coq_Proposition option **)

let rec coq_Search_for_related target resource =
  match target with
  | Up p -> coq_Search_for_related p resource
  | Bp (_, p1, p2) ->
    (match coq_Search_for_related p1 resource with
     | Some c -> Some c
     | None -> coq_Search_for_related p2 resource)
  | Ue (_, e) -> coq_Find_prop_in_used_list e resource
  | Be (_, e1, e2) ->
    (match coq_Find_prop_in_used_list e1 resource with
     | Some c -> Some c
     | None -> coq_Find_prop_in_used_list e2 resource)
  | In_bound (_, val0, _) ->
    coq_Find_prop_in_used_list (coq_Main_bounded val0) resource
  | Qf (_, _, p) -> coq_Search_for_related p resource
  | _ -> if coq_Find_Prop_in_list resource target then Some target else None

(** val coq_Divisor_reduct : coq_Proposition -> coq_Proposition **)

let coq_Divisor_reduct target = match target with
| Be (op, e1, e2) ->
  (match e1 with
   | Vbop (b, a1, a2) ->
     (match b with
      | Omul ->
        (match e2 with
         | Vbop (b0, b1, b2) ->
           (match b0 with
            | Omul ->
              let newop =
                match coq_Get_neg_be_op op with
                | Some c -> c
                | None -> op
              in
              if eqb_val a1 b1
              then if coq_Be_eqb newop op
                   then Bp (Pand, (Up (Be (Pvequal, a1, (Ez_val Z0)))), (Be
                          (op, a2, b2)))
                   else Bp (Por, (Bp (Pand, (Be (Pvgt, a1, (Ez_val Z0))), (Be
                          (op, a2, b2)))), (Bp (Pand, (Be (Pvlt, a1, (Ez_val
                          Z0))), (Be (newop, a2, b2)))))
              else if eqb_val a1 b2
                   then if coq_Be_eqb newop op
                        then Bp (Pand, (Up (Be (Pvequal, a1, (Ez_val Z0)))),
                               (Be (op, a2, b1)))
                        else Bp (Por, (Bp (Pand, (Be (Pvgt, a1, (Ez_val
                               Z0))), (Be (op, a2, b1)))), (Bp (Pand, (Be
                               (Pvlt, a1, (Ez_val Z0))), (Be (newop, a2,
                               b1)))))
                   else if eqb_val a2 b1
                        then if coq_Be_eqb newop op
                             then Bp (Pand, (Up (Be (Pvequal, a2, (Ez_val
                                    Z0)))), (Be (op, a1, b2)))
                             else Bp (Por, (Bp (Pand, (Be (Pvgt, a2, (Ez_val
                                    Z0))), (Be (op, a1, b2)))), (Bp (Pand,
                                    (Be (Pvlt, a2, (Ez_val Z0))), (Be (newop,
                                    a1, b2)))))
                        else if eqb_val a2 b2
                             then if coq_Be_eqb newop op
                                  then Bp (Pand, (Up (Be (Pvequal, a2,
                                         (Ez_val Z0)))), (Be (op, a1, b1)))
                                  else Bp (Por, (Bp (Pand, (Be (Pvgt, a2,
                                         (Ez_val Z0))), (Be (op, a1, b1)))),
                                         (Bp (Pand, (Be (Pvlt, a2, (Ez_val
                                         Z0))), (Be (newop, a1, b1)))))
                             else Be (op, e1, e2)
            | _ -> Be (op, e1, e2))
         | _ -> Be (op, e1, e2))
      | _ -> Be (op, e1, e2))
   | _ -> Be (op, e1, e2))
| _ -> target

(** val coq_Prop_solve :
    coq_Proposition -> coq_Proposition list -> nat -> bool option **)

let rec coq_Prop_solve target resource = function
| O -> None
| S n ->
  (match coq_Divisor_reduct target with
   | Up p -> option_not (coq_Prop_solve p resource n)
   | Bp (op, p1, p2) ->
     (match op with
      | Por ->
        (match option_or (coq_Prop_solve p1 resource n)
                 (coq_Prop_solve p2 resource n) with
         | Some c -> Some c
         | None ->
           (match coq_Search_for_related target resource with
            | Some p ->
              (match p with
               | Bp (b, p3, p4) ->
                 (match b with
                  | Por ->
                    option_and
                      (coq_Prop_solve target
                        (p3 :: (coq_Remove coq_Proposition_eqb resource p)) n)
                      (coq_Prop_solve target
                        (p4 :: (coq_Remove coq_Proposition_eqb resource p)) n)
                  | _ ->
                    coq_Prop_solve target
                      (coq_Remove coq_Proposition_eqb resource p) n)
               | _ ->
                 coq_Prop_solve target
                   (coq_Remove coq_Proposition_eqb resource p) n)
            | None -> None))
      | Pand ->
        option_and (coq_Prop_solve p1 resource n)
          (coq_Prop_solve p2 resource n)
      | Pimply ->
        option_or (option_not (coq_Prop_solve p1 resource n))
          (coq_Prop_solve p2 (p1 :: resource) n)
      | Piff ->
        option_and (coq_Prop_solve p1 (p2 :: resource) n)
          (coq_Prop_solve p2 (p1 :: resource) n))
   | Ue (op, e) ->
     (match op with
      | Pisptr -> coq_Pisptr_solve e resource n
      | Pis_pointer_or_null -> coq_Pis_pointer_or_null_solve e resource n)
   | Be (op, e1, e2) ->
     (match op with
      | Pvle -> coq_Pvle_solve e1 e2 resource n
      | Pvge -> coq_Pvge_solve e1 e2 resource n
      | Pvlt -> coq_Pvlt_solve e1 e2 resource n
      | Pvgt -> coq_Pvgt_solve e1 e2 resource n
      | Pvequal ->
        option_or (coq_Pvequal_solve e1 e2 resource n)
          (coq_Pvequal_solve e2 e1 resource n)
      | _ ->
        (match coq_Search_for_related target resource with
         | Some p ->
           let (flag, result) = coq_Try_solve target p in
           (match result with
            | Some c' ->
              option_and
                (coq_Prop_solve c'
                  (coq_Remove coq_Proposition_eqb resource p) n)
                (coq_Prop_solve target
                  (coq_Remove coq_Proposition_eqb resource p) n)
            | None -> Some flag)
         | None -> None))
   | In_bound (low, val0, high) ->
     option_and
       (coq_Prop_solve (Be (Pvle, low, (coq_Main_bounded val0))) resource n)
       (coq_Prop_solve (Be (Pvle, (coq_Main_bounded val0), high)) resource n)
   | Qf (_, i, p) ->
     (match p with
      | Bp (b, p1, p2) ->
        (match b with
         | Pimply ->
           (match coq_Find_prop_in_used_list i resource with
            | Some c ->
              (match c with
               | Qf (q, _, p0) ->
                 (match q with
                  | PForall ->
                    (match p0 with
                     | Bp (b0, p1', p2') ->
                       (match b0 with
                        | Pimply ->
                          if coq_Proposition_eqb p2 p2'
                          then (match p1 with
                                | Bp (b1, p3, p4) ->
                                  (match b1 with
                                   | Pand ->
                                     (match p3 with
                                      | Be (b2, l, i0) ->
                                        (match b2 with
                                         | Pvle ->
                                           (match p4 with
                                            | Be (b3, i1, r) ->
                                              (match b3 with
                                               | Pvle ->
                                                 (match p1' with
                                                  | Bp (b4, p5, p6) ->
                                                    (match b4 with
                                                     | Pand ->
                                                       (match p5 with
                                                        | Be (b5, l', i') ->
                                                          (match b5 with
                                                           | Pvle ->
                                                             (match p6 with
                                                              | Be (b6, i1',
                                                                    r') ->
                                                                (match b6 with
                                                                 | Pvle ->
                                                                   if 
                                                                    (&&)
                                                                    (eqb_val
                                                                    i0 i1)
                                                                    (eqb_val
                                                                    i' i1')
                                                                   then 
                                                                    option_and
                                                                    (coq_Prop_solve
                                                                    (Qf
                                                                    (PForall,
                                                                    i0, (Bp
                                                                    (Pimply,
                                                                    (Bp
                                                                    (Pand,
                                                                    (Be
                                                                    (Pvle, l,
                                                                    i0)), (Be
                                                                    (Pvlt,
                                                                    i0,
                                                                    l')))),
                                                                    p2))))
                                                                    (coq_Remove
                                                                    coq_Proposition_eqb
                                                                    resource
                                                                    c) n)
                                                                    (coq_Prop_solve
                                                                    (Qf
                                                                    (PForall,
                                                                    i0, (Bp
                                                                    (Pimply,
                                                                    (Bp
                                                                    (Pand,
                                                                    (Be
                                                                    (Pvlt,
                                                                    r', i0)),
                                                                    (Be
                                                                    (Pvle,
                                                                    i0,
                                                                    r)))),
                                                                    p2))))
                                                                    (coq_Remove
                                                                    coq_Proposition_eqb
                                                                    resource
                                                                    c) n)
                                                                   else None
                                                                 | _ -> None)
                                                              | _ -> None)
                                                           | _ -> None)
                                                        | _ -> None)
                                                     | _ -> None)
                                                  | _ -> None)
                                               | _ -> None)
                                            | _ -> None)
                                         | _ -> None)
                                      | _ -> None)
                                   | _ -> None)
                                | _ -> None)
                          else coq_Prop_solve target
                                 (coq_Remove coq_Proposition_eqb resource c) n
                        | _ ->
                          coq_Prop_solve target
                            (coq_Remove coq_Proposition_eqb resource c) n)
                     | _ ->
                       coq_Prop_solve target
                         (coq_Remove coq_Proposition_eqb resource c) n)
                  | PExists ->
                    coq_Prop_solve target
                      (coq_Remove coq_Proposition_eqb resource c) n)
               | _ ->
                 coq_Prop_solve target
                   (coq_Remove coq_Proposition_eqb resource c) n)
            | None ->
              (match p1 with
               | Bp (b0, p0, p3) ->
                 (match b0 with
                  | Pand ->
                    (match p0 with
                     | Be (b1, l, i0) ->
                       (match b1 with
                        | Pvle ->
                          (match p3 with
                           | Be (b2, i1, r) ->
                             (match b2 with
                              | Pvle ->
                                if (&&) (eqb_val i0 i1) (eqb_val r l)
                                then coq_Prop_solve
                                       (coq_Changeval_prop i0 r p2) resource n
                                else None
                              | Pvlt ->
                                if (&&) (eqb_val i0 i1) (eqb_val l r)
                                then Some true
                                else if formal_expr_val_eqb
                                          (coq_Transfer_expr_val (Vbop (Oadd,
                                            l, (Ez_val (Zpos Coq_xH)))))
                                          (coq_Transfer_expr_val r)
                                     then coq_Prop_solve
                                            (coq_Changeval_prop i0 l p2)
                                            resource n
                                     else None
                              | _ -> None)
                           | _ -> None)
                        | Pvlt ->
                          (match p3 with
                           | Be (b2, i1, r) ->
                             (match b2 with
                              | Pvle ->
                                if (&&) (eqb_val i0 i1) (eqb_val l r)
                                then Some true
                                else if formal_expr_val_eqb
                                          (coq_Transfer_expr_val (Vbop (Oadd,
                                            l, (Ez_val (Zpos Coq_xH)))))
                                          (coq_Transfer_expr_val r)
                                     then coq_Prop_solve
                                            (coq_Changeval_prop i0 r p2)
                                            resource n
                                     else None
                              | _ -> None)
                           | _ -> None)
                        | _ -> None)
                     | _ -> None)
                  | _ -> None)
               | _ -> None))
         | _ -> None)
      | _ -> None)
   | _ ->
     (match coq_Search_for_related target resource with
      | Some p ->
        let (flag, result) = coq_Try_solve target p in
        (match result with
         | Some c' ->
           option_and
             (coq_Prop_solve c' (coq_Remove coq_Proposition_eqb resource p) n)
             (coq_Prop_solve target
               (coq_Remove coq_Proposition_eqb resource p) n)
         | None -> Some flag)
      | None -> None))

(** val coq_Prop_solver :
    coq_Proposition list -> coq_Proposition list -> nat -> coq_Proposition
    list option **)

let rec coq_Prop_solver targetx resourcex fuel =
  match targetx with
  | [] -> Some []
  | pG :: lG' ->
    if coq_Find coq_Proposition_eqb resourcex pG
    then coq_Prop_solver lG' resourcex fuel
    else (match coq_Prop_solve pG resourcex
                  (add (mul (S (S O)) (length resourcex)) fuel) with
          | Some b -> if b then coq_Prop_solver lG' resourcex fuel else None
          | None ->
            (match coq_Prop_solver lG' resourcex fuel with
             | Some l -> Some (pG :: l)
             | None -> None))

(** val coq_Prop_list_solver :
    coq_Proposition list -> coq_Proposition list -> coq_Proposition list
    option **)

let coq_Prop_list_solver targetx resourcex =
  coq_Prop_solver (simpl_Proposition_list targetx)
    (simpl_Proposition_list resourcex)
    (length (simpl_Proposition_list targetx))
