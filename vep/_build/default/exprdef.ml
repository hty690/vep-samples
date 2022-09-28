open BinInt
open BinNums
open BinPos
open Datatypes
open List_lemma

type unary_operation =
| Oneg
| Onot
| Shl32
| Right32

(** val eqb_unary : unary_operation -> unary_operation -> bool **)

let eqb_unary a b =
  match a with
  | Oneg -> (match b with
             | Oneg -> true
             | _ -> false)
  | Onot -> (match b with
             | Onot -> true
             | _ -> false)
  | Shl32 -> (match b with
              | Shl32 -> true
              | _ -> false)
  | Right32 -> (match b with
                | Right32 -> true
                | _ -> false)

type binary_operation =
| Oadd
| Osub
| Omul
| Opower
| Odiv
| Omod
| Oand
| Oor
| Oxor
| Oshl
| Oshr
| OAshl
| OAshr
| Oeq
| One
| Oend

(** val eqb_binary : binary_operation -> binary_operation -> bool **)

let eqb_binary a b =
  match a with
  | Oadd -> (match b with
             | Oadd -> true
             | _ -> false)
  | Osub -> (match b with
             | Osub -> true
             | _ -> false)
  | Omul -> (match b with
             | Omul -> true
             | _ -> false)
  | Opower -> (match b with
               | Opower -> true
               | _ -> false)
  | Odiv -> (match b with
             | Odiv -> true
             | _ -> false)
  | Omod -> (match b with
             | Omod -> true
             | _ -> false)
  | Oand -> (match b with
             | Oand -> true
             | _ -> false)
  | Oor -> (match b with
            | Oor -> true
            | _ -> false)
  | Oxor -> (match b with
             | Oxor -> true
             | _ -> false)
  | Oshl -> (match b with
             | Oshl -> true
             | _ -> false)
  | Oshr -> (match b with
             | Oshr -> true
             | _ -> false)
  | OAshl -> (match b with
              | OAshl -> true
              | _ -> false)
  | OAshr -> (match b with
              | OAshr -> true
              | _ -> false)
  | Oeq -> (match b with
            | Oeq -> true
            | _ -> false)
  | One -> (match b with
            | One -> true
            | _ -> false)
  | Oend -> (match b with
             | Oend -> true
             | _ -> false)

type list_operation =
| Vnthaddress
| Vnthval
| Vnth
| Vcons
| Vapp

(** val eqb_listop : list_operation -> list_operation -> bool **)

let eqb_listop a b =
  match a with
  | Vnthaddress -> (match b with
                    | Vnthaddress -> true
                    | _ -> false)
  | Vnthval -> (match b with
                | Vnthval -> true
                | _ -> false)
  | Vnth -> (match b with
             | Vnth -> true
             | _ -> false)
  | Vcons -> (match b with
              | Vcons -> true
              | _ -> false)
  | Vapp -> (match b with
             | Vapp -> true
             | _ -> false)

type expr_val =
| V_vari of ident
| Vfield_address of expr_val * ident
| Ez_val of coq_Z
| Vuop of unary_operation * expr_val
| Vbop of binary_operation * expr_val * expr_val
| Vlop of list_operation * expr_val * expr_val
| Vlist_vari of ident
| Vlist_sublist of expr_val * expr_val * expr_val
| Vlist_length of expr_val
| Vif of expr_val * expr_val * expr_val

(** val nullptr : expr_val **)

let nullptr =
  Ez_val Z0

(** val coq_Vtrue : expr_val **)

let coq_Vtrue =
  Ez_val (Zpos Coq_xH)

(** val coq_Vfalse : expr_val **)

let coq_Vfalse =
  Ez_val Z0

(** val get_val : expr_val -> expr_val **)

let rec get_val v = match v with
| Vfield_address (v', _) -> get_val v'
| _ -> v

(** val eqb_val : expr_val -> expr_val -> bool **)

let rec eqb_val x1 x2 =
  match x1 with
  | V_vari a -> (match x2 with
                 | V_vari b -> Pos.eqb a b
                 | _ -> false)
  | Vfield_address (a, a1) ->
    (match x2 with
     | Vfield_address (b, b1) -> (&&) (eqb_val a b) (Pos.eqb a1 b1)
     | _ -> false)
  | Ez_val a -> (match x2 with
                 | Ez_val b -> Z.eqb a b
                 | _ -> false)
  | Vuop (op, a) ->
    (match x2 with
     | Vuop (op', a') -> (&&) (eqb_unary op op') (eqb_val a a')
     | _ -> false)
  | Vbop (op, a, b) ->
    (match x2 with
     | Vbop (op', a', b') ->
       (&&) ((&&) (eqb_binary op op') (eqb_val a a')) (eqb_val b b')
     | _ -> false)
  | Vlop (op, a, b) ->
    (match x2 with
     | Vlop (op', a', b') ->
       (&&) ((&&) (eqb_listop op op') (eqb_val a a')) (eqb_val b b')
     | _ -> false)
  | Vlist_vari a -> (match x2 with
                     | Vlist_vari b -> Pos.eqb a b
                     | _ -> false)
  | Vlist_sublist (l, r, s) ->
    (match x2 with
     | Vlist_sublist (l', r', s') ->
       (&&) ((&&) (eqb_val l l') (eqb_val r r')) (eqb_val s s')
     | _ -> false)
  | Vlist_length _ -> false
  | Vif (a, b, c) ->
    (match x2 with
     | Vif (a', b', c') ->
       (&&) ((&&) (eqb_val a a') (eqb_val b b')) (eqb_val c c')
     | _ -> false)

(** val coq_Find_list_val : expr_val -> bool **)

let rec coq_Find_list_val = function
| Vfield_address (v', _) -> coq_Find_list_val v'
| Vuop (_, a) -> coq_Find_list_val a
| Vbop (_, a, b) -> (||) (coq_Find_list_val a) (coq_Find_list_val b)
| Vlop (_, a, b) -> (||) (coq_Find_list_val a) (coq_Find_list_val b)
| Vlist_vari _ -> true
| Vlist_sublist (_, _, s) -> coq_Find_list_val s
| _ -> false

(** val coq_Used_in_val : expr_val -> expr_val -> bool **)

let rec coq_Used_in_val v w =
  (||) (eqb_val v w)
    (match v with
     | Vfield_address (v', _) -> coq_Used_in_val v' w
     | Vuop (_, a) -> coq_Used_in_val a w
     | Vbop (_, a, b) -> (||) (coq_Used_in_val a w) (coq_Used_in_val b w)
     | Vlop (_, a, b) -> (||) (coq_Used_in_val a w) (coq_Used_in_val b w)
     | Vlist_sublist (l, r, s) ->
       (||) ((||) (coq_Used_in_val l w) (coq_Used_in_val r w))
         (coq_Used_in_val s w)
     | Vif (a, b, c) ->
       (||) ((||) (coq_Used_in_val a w) (coq_Used_in_val b w))
         (coq_Used_in_val c w)
     | _ -> false)

(** val coq_Changeval : expr_val -> expr_val -> expr_val -> expr_val **)

let rec coq_Changeval x y z = match z with
| V_vari _ -> if eqb_val x z then y else z
| Vfield_address (a, b) ->
  (match x with
   | V_vari xi ->
     (match y with
      | V_vari yi ->
        Vfield_address ((coq_Changeval x y a),
          (if Pos.eqb xi b then yi else b))
      | _ -> Vfield_address (a, b))
   | _ -> Vfield_address (a, b))
| Ez_val _ -> z
| Vuop (op, a) -> Vuop (op, (coq_Changeval x y a))
| Vbop (op, a, b) -> Vbop (op, (coq_Changeval x y a), (coq_Changeval x y b))
| Vlop (op, a, b) -> Vlop (op, (coq_Changeval x y a), (coq_Changeval x y b))
| Vlist_vari a ->
  (match x with
   | V_vari xi ->
     (match y with
      | V_vari yi -> Vlist_vari (if Pos.eqb xi a then yi else a)
      | Vlist_vari yi -> Vlist_vari (if Pos.eqb xi a then yi else a)
      | _ -> Vlist_vari a)
   | _ -> Vlist_vari a)
| Vlist_sublist (a, b, c) ->
  Vlist_sublist ((coq_Changeval x y a), (coq_Changeval x y b),
    (coq_Changeval x y c))
| Vlist_length a -> Vlist_length (coq_Changeval x y a)
| Vif (a, b, c) ->
  Vif ((coq_Changeval x y a), (coq_Changeval x y b), (coq_Changeval x y c))

(** val coq_Changeval_option :
    expr_val -> expr_val -> expr_val option -> expr_val option **)

let coq_Changeval_option x y = function
| Some z' -> Some (coq_Changeval x y z')
| None -> None

(** val coq_Simplify_uop : unary_operation -> expr_val -> expr_val **)

let coq_Simplify_uop op x =
  match op with
  | Oneg ->
    (match x with
     | Ez_val b -> Ez_val (Z.opp b)
     | Vuop (u, x') -> (match u with
                        | Oneg -> x'
                        | _ -> Vuop (Oneg, x))
     | _ -> Vuop (Oneg, x))
  | Onot ->
    (match x with
     | Ez_val a -> if Z.eqb a Z0 then coq_Vtrue else coq_Vfalse
     | Vuop (u, x') -> (match u with
                        | Onot -> x'
                        | _ -> Vuop (Onot, x))
     | Vbop (b, a', b') ->
       (match b with
        | Oeq -> Vbop (One, a', b')
        | One -> Vbop (Oeq, a', b')
        | _ -> Vuop (Onot, x))
     | _ -> Vuop (Onot, x))
  | Shl32 ->
    (match x with
     | Vuop (u, _) -> (match u with
                       | Shl32 -> x
                       | _ -> Vuop (Shl32, x))
     | _ -> Vuop (Shl32, x))
  | Right32 ->
    (match x with
     | Vuop (u, _) -> (match u with
                       | Right32 -> x
                       | _ -> Vuop (Right32, x))
     | _ -> Vuop (Right32, x))

(** val coq_Simplify_bop :
    binary_operation -> expr_val -> expr_val -> expr_val **)

let coq_Simplify_bop op x y =
  match op with
  | Oadd ->
    (match x with
     | Ez_val a1 ->
       (match a1 with
        | Z0 ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.add a1 b1)
           | Vlop (l, a, s) ->
             (match l with
              | Vnth -> Vlop (Vnth, (Vbop (Oadd, a, x)), s)
              | _ -> y)
           | Vlist_vari a2 -> Vlop (Vnth, x, (Vlist_vari a2))
           | _ -> y)
        | _ ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.add a1 b1)
           | Vlop (l, a, s) ->
             (match l with
              | Vnth -> Vlop (Vnth, (Vbop (Oadd, a, x)), s)
              | _ -> Vbop (Oadd, x, y))
           | Vlist_vari a2 -> Vlop (Vnth, x, (Vlist_vari a2))
           | _ -> Vbop (Oadd, x, y)))
     | Vlop (l, a, s) ->
       (match l with
        | Vnth ->
          (match y with
           | Vlist_vari a1 -> Vlop (Vnth, x, (Vlist_vari a1))
           | _ -> Vlop (Vnth, (Vbop (Oadd, a, y)), s))
        | _ ->
          (match y with
           | Ez_val z -> (match z with
                          | Z0 -> x
                          | _ -> Vbop (Oadd, x, y))
           | Vlop (l0, a0, s0) ->
             (match l0 with
              | Vnth -> Vlop (Vnth, (Vbop (Oadd, a0, x)), s0)
              | _ -> Vbop (Oadd, x, y))
           | Vlist_vari a1 -> Vlop (Vnth, x, (Vlist_vari a1))
           | _ -> Vbop (Oadd, x, y)))
     | Vlist_vari a1 -> Vlop (Vnth, y, (Vlist_vari a1))
     | _ ->
       (match y with
        | Ez_val z -> (match z with
                       | Z0 -> x
                       | _ -> Vbop (Oadd, x, y))
        | Vlop (l, a, s) ->
          (match l with
           | Vnth -> Vlop (Vnth, (Vbop (Oadd, a, x)), s)
           | _ -> Vbop (Oadd, x, y))
        | Vlist_vari a1 -> Vlop (Vnth, x, (Vlist_vari a1))
        | _ -> Vbop (Oadd, x, y)))
  | Osub ->
    (match x with
     | Ez_val a1 ->
       (match a1 with
        | Z0 ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.sub a1 b1)
           | _ -> Vuop (Oneg, y))
        | _ ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.sub a1 b1)
           | _ -> Vbop (Osub, x, y)))
     | _ ->
       (match y with
        | Ez_val z -> (match z with
                       | Z0 -> x
                       | _ -> Vbop (Osub, x, y))
        | _ -> Vbop (Osub, x, y)))
  | Omul ->
    (match x with
     | Ez_val a1 ->
       (match a1 with
        | Z0 ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.mul a1 b1)
           | _ -> Ez_val Z0)
        | Zpos p ->
          (match p with
           | Coq_xH ->
             (match y with
              | Ez_val b1 -> Ez_val (Z.mul a1 b1)
              | _ -> y)
           | _ ->
             (match y with
              | Ez_val b1 -> Ez_val (Z.mul a1 b1)
              | _ -> Vbop (Omul, x, y)))
        | Zneg _ ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.mul a1 b1)
           | _ -> Vbop (Omul, x, y)))
     | _ ->
       (match y with
        | Ez_val z ->
          (match z with
           | Z0 -> Ez_val Z0
           | Zpos p -> (match p with
                        | Coq_xH -> x
                        | _ -> Vbop (Omul, x, y))
           | Zneg _ -> Vbop (Omul, x, y))
        | _ -> Vbop (Omul, x, y)))
  | Opower ->
    (match x with
     | Ez_val a1 ->
       (match a1 with
        | Z0 ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.pow a1 b1)
           | _ -> Ez_val Z0)
        | Zpos p ->
          (match p with
           | Coq_xH ->
             (match y with
              | Ez_val b1 -> Ez_val (Z.pow a1 b1)
              | _ -> Ez_val (Zpos Coq_xH))
           | _ ->
             (match y with
              | Ez_val b1 -> Ez_val (Z.pow a1 b1)
              | Vbop (b0, a, b) ->
                (match b0 with
                 | Oadd ->
                   Vbop (Omul, (Vbop (Opower, x, a)), (Vbop (Opower, x, b)))
                 | _ -> Vbop (Opower, x, y))
              | _ -> Vbop (Opower, x, y)))
        | Zneg _ ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.pow a1 b1)
           | Vbop (b0, a, b) ->
             (match b0 with
              | Oadd ->
                Vbop (Omul, (Vbop (Opower, x, a)), (Vbop (Opower, x, b)))
              | _ -> Vbop (Opower, x, y))
           | _ -> Vbop (Opower, x, y)))
     | Vbop (_, _, _) ->
       (match y with
        | Ez_val z ->
          (match z with
           | Z0 -> Ez_val (Zpos Coq_xH)
           | Zpos p -> (match p with
                        | Coq_xH -> x
                        | _ -> Vbop (Opower, x, y))
           | Zneg _ -> Vbop (Opower, x, y))
        | Vbop (b1, a, b) ->
          (match b1 with
           | Oadd -> Vbop (Omul, (Vbop (Opower, x, a)), (Vbop (Opower, x, b)))
           | _ -> Vbop (Opower, x, y))
        | _ -> Vbop (Opower, x, y))
     | _ ->
       (match y with
        | Ez_val z ->
          (match z with
           | Z0 -> Ez_val (Zpos Coq_xH)
           | Zpos p -> (match p with
                        | Coq_xH -> x
                        | _ -> Vbop (Opower, x, y))
           | Zneg _ -> Vbop (Opower, x, y))
        | Vbop (b0, a, b) ->
          (match b0 with
           | Oadd -> Vbop (Omul, (Vbop (Opower, x, a)), (Vbop (Opower, x, b)))
           | _ -> Vbop (Opower, x, y))
        | _ -> Vbop (Opower, x, y)))
  | Odiv ->
    (match x with
     | Ez_val a1 ->
       (match a1 with
        | Z0 ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.div a1 b1)
           | _ -> Ez_val Z0)
        | _ ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.div a1 b1)
           | _ -> Vbop (Odiv, x, y)))
     | _ -> Vbop (Odiv, x, y))
  | Omod ->
    (match x with
     | Ez_val a1 ->
       (match a1 with
        | Z0 ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.modulo a1 b1)
           | _ -> Ez_val Z0)
        | _ ->
          (match y with
           | Ez_val b1 -> Ez_val (Z.modulo a1 b1)
           | _ -> Vbop (Omod, x, y)))
     | _ -> Vbop (Omod, x, y))
  | Oshl ->
    (match y with
     | Ez_val t ->
       if Z.eqb t Z0
       then x
       else if Z.eqb t (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                 Coq_xH))))))
            then Vuop (Shl32, x)
            else Vbop (Omul, x, (Ez_val (Z.pow (Zpos (Coq_xO Coq_xH)) t)))
     | _ -> Vbop (Oshl, x, y))
  | Oshr ->
    (match x with
     | Ez_val _ ->
       (match y with
        | Ez_val z0 -> (match z0 with
                        | Z0 -> x
                        | _ -> Vbop (Oshr, x, y))
        | _ -> Vbop (Oshr, x, y))
     | Vuop (u, x') ->
       (match u with
        | Shl32 ->
          (match y with
           | Ez_val z ->
             (match z with
              | Z0 -> x
              | Zpos p ->
                (match p with
                 | Coq_xO p0 ->
                   (match p0 with
                    | Coq_xO p1 ->
                      (match p1 with
                       | Coq_xO p2 ->
                         (match p2 with
                          | Coq_xO p3 ->
                            (match p3 with
                             | Coq_xO p4 ->
                               (match p4 with
                                | Coq_xH -> Vuop (Right32, x')
                                | _ -> Vbop (Oshr, x, y))
                             | _ -> Vbop (Oshr, x, y))
                          | _ -> Vbop (Oshr, x, y))
                       | _ -> Vbop (Oshr, x, y))
                    | _ -> Vbop (Oshr, x, y))
                 | _ -> Vbop (Oshr, x, y))
              | Zneg _ -> Vbop (Oshr, x, y))
           | _ -> Vbop (Oshr, x, y))
        | _ ->
          (match y with
           | Ez_val z -> (match z with
                          | Z0 -> x
                          | _ -> Vbop (Oshr, x, y))
           | _ -> Vbop (Oshr, x, y)))
     | _ ->
       (match y with
        | Ez_val z -> (match z with
                       | Z0 -> x
                       | _ -> Vbop (Oshr, x, y))
        | _ -> Vbop (Oshr, x, y)))
  | OAshl ->
    (match y with
     | Ez_val t ->
       if Z.eqb t Z0
       then x
       else if Z.eqb t (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                 Coq_xH))))))
            then Vuop (Shl32, x)
            else Vbop (Omul, x, (Ez_val (Z.pow (Zpos (Coq_xO Coq_xH)) t)))
     | _ -> Vbop (Oshl, x, y))
  | OAshr ->
    (match x with
     | Ez_val _ ->
       (match y with
        | Ez_val z0 -> (match z0 with
                        | Z0 -> x
                        | _ -> Vbop (OAshr, x, y))
        | _ -> Vbop (OAshr, x, y))
     | Vuop (u, x') ->
       (match u with
        | Shl32 ->
          (match y with
           | Ez_val z ->
             (match z with
              | Z0 -> x
              | Zpos p ->
                (match p with
                 | Coq_xO p0 ->
                   (match p0 with
                    | Coq_xO p1 ->
                      (match p1 with
                       | Coq_xO p2 ->
                         (match p2 with
                          | Coq_xO p3 ->
                            (match p3 with
                             | Coq_xO p4 ->
                               (match p4 with
                                | Coq_xH -> x'
                                | _ -> Vbop (OAshr, x, y))
                             | _ -> Vbop (OAshr, x, y))
                          | _ -> Vbop (OAshr, x, y))
                       | _ -> Vbop (OAshr, x, y))
                    | _ -> Vbop (OAshr, x, y))
                 | _ -> Vbop (OAshr, x, y))
              | Zneg _ -> Vbop (OAshr, x, y))
           | _ -> Vbop (OAshr, x, y))
        | _ ->
          (match y with
           | Ez_val z -> (match z with
                          | Z0 -> x
                          | _ -> Vbop (OAshr, x, y))
           | _ -> Vbop (OAshr, x, y)))
     | _ ->
       (match y with
        | Ez_val z -> (match z with
                       | Z0 -> x
                       | _ -> Vbop (OAshr, x, y))
        | _ -> Vbop (OAshr, x, y)))
  | Oeq ->
    if eqb_val x y
    then coq_Vtrue
    else (match x with
          | Ez_val a1 ->
            (match y with
             | Ez_val b1 -> if Z.eqb a1 b1 then coq_Vtrue else coq_Vfalse
             | _ -> Vbop (Oeq, x, y))
          | _ -> Vbop (Oeq, x, y))
  | One ->
    if eqb_val x y
    then coq_Vfalse
    else (match x with
          | Ez_val a1 ->
            (match y with
             | Ez_val b1 -> if Z.eqb a1 b1 then coq_Vfalse else coq_Vtrue
             | _ -> Vbop (One, x, y))
          | _ -> Vbop (One, x, y))
  | _ -> Vbop (op, x, y)

(** val coq_Simplify_lop :
    list_operation -> expr_val -> expr_val -> expr_val **)

let coq_Simplify_lop op x y =
  match op with
  | Vnth ->
    (match y with
     | Vlist_sublist (l, _, s) -> Vlop (Vnth, (coq_Simplify_bop Oadd x l), s)
     | _ -> Vlop (Vnth, x, y))
  | Vapp ->
    (match x with
     | Vlop (l, _, _) ->
       (match l with
        | Vnth -> Vlop (Vcons, x, y)
        | _ -> Vlop (Vapp, x, y))
     | _ -> Vlop (Vapp, x, y))
  | _ -> Vlop (op, x, y)

(** val coq_Simplify_val_rec : expr_val -> expr_val **)

let rec coq_Simplify_val_rec x = match x with
| Vfield_address (v', id') -> Vfield_address ((coq_Simplify_val_rec v'), id')
| Ez_val a -> Ez_val a
| Vuop (op, a) -> coq_Simplify_uop op (coq_Simplify_val_rec a)
| Vbop (op, a, b) ->
  coq_Simplify_bop op (coq_Simplify_val_rec a) (coq_Simplify_val_rec b)
| Vlop (op, a, b) ->
  coq_Simplify_lop op (coq_Simplify_val_rec a) (coq_Simplify_val_rec b)
| Vlist_sublist (l, r, s) ->
  let l' = coq_Simplify_val_rec l in
  let r' = coq_Simplify_val_rec r in
  let s' = coq_Simplify_val_rec s in
  if eqb_val l' r' then Vlop (Vnth, l', s') else Vlist_sublist (l', r', s')
| _ -> x

(** val coq_Simplify_val_ser : expr_val -> nat -> expr_val **)

let rec coq_Simplify_val_ser x fuel =
  let sx = coq_Simplify_val_rec x in
  (match fuel with
   | O -> sx
   | S n -> if eqb_val x sx then sx else coq_Simplify_val_ser sx n)

(** val coq_Simplify_val : expr_val -> expr_val **)

let coq_Simplify_val x =
  coq_Simplify_val_ser x (S (S (S (S (S (S (S (S (S (S O))))))))))

(** val max_ident_in_expr : expr_val -> ident **)

let rec max_ident_in_expr = function
| V_vari c -> c
| Vfield_address (x', _) -> max_ident_in_expr x'
| Ez_val _ -> Coq_xH
| Vuop (_, x') -> max_ident_in_expr x'
| Vbop (_, x1, x2) -> Pos.max (max_ident_in_expr x1) (max_ident_in_expr x2)
| Vlop (_, x1, x2) -> Pos.max (max_ident_in_expr x1) (max_ident_in_expr x2)
| Vlist_vari c -> c
| Vlist_sublist (x1, x2, x3) ->
  Pos.max (max_ident_in_expr x1)
    (Pos.max (max_ident_in_expr x2) (max_ident_in_expr x3))
| Vlist_length x1 -> max_ident_in_expr x1
| Vif (x1, x2, x3) ->
  Pos.max (max_ident_in_expr x1)
    (Pos.max (max_ident_in_expr x2) (max_ident_in_expr x3))

type formal_part = expr_val list * coq_Z

type formal_expr_val = formal_part list

(** val formal_part_eqb : formal_part -> formal_part -> bool **)

let formal_part_eqb a b =
  (&&) (eqb_list eqb_val (fst a) (fst b)) (Z.eqb (snd a) (snd b))

(** val formal_expr_val_eqb : formal_part list -> formal_part list -> bool **)

let formal_expr_val_eqb =
  eqb_list formal_part_eqb

(** val coq_Retransfer_part : expr_val list -> expr_val **)

let rec coq_Retransfer_part = function
| [] -> Ez_val (Zpos Coq_xH)
| a :: s' -> Vbop (Omul, a, (coq_Retransfer_part s'))

(** val coq_Retransfer_formal : formal_expr_val -> expr_val **)

let rec coq_Retransfer_formal = function
| [] -> Ez_val Z0
| f :: s' ->
  let (a, b) = f in
  Vbop (Oadd, (Vbop (Omul, (coq_Retransfer_part a), (Ez_val b))),
  (coq_Retransfer_formal s'))

(** val coq_Retransfer : formal_expr_val -> expr_val **)

let coq_Retransfer s =
  coq_Simplify_val (coq_Retransfer_formal s)

(** val coq_Neg_transfer_part : formal_part -> formal_part **)

let coq_Neg_transfer_part = function
| (a, b) -> (a, (Z.opp b))

(** val coq_Neg_transfer : formal_expr_val -> formal_expr_val **)

let rec coq_Neg_transfer = function
| [] -> []
| a :: s' -> (coq_Neg_transfer_part a) :: (coq_Neg_transfer s')

(** val coq_Add_transfer :
    formal_expr_val -> formal_expr_val -> formal_expr_val **)

let rec coq_Add_transfer s1 s2 =
  match s1 with
  | [] -> s2
  | a :: s' ->
    let s2' = coq_Add_transfer s' s2 in
    (match coq_Find_A_in_prodAB (eqb_list eqb_val) s2' (fst a) with
     | Some c1 ->
       ((fst a),
         (Z.add (snd a) c1)) :: (remove_once formal_part_eqb s2' ((fst a),
                                  c1))
     | None -> a :: s2')

(** val coq_Sub_transfer :
    formal_expr_val -> formal_expr_val -> formal_expr_val **)

let coq_Sub_transfer s1 s2 =
  coq_Add_transfer s1 (coq_Neg_transfer s2)

(** val coq_Mul_one_one : formal_part -> formal_part -> formal_part **)

let coq_Mul_one_one s1 s2 =
  ((app (fst s1) (fst s2)), (Z.mul (snd s1) (snd s2)))

(** val coq_Mul_one_transfer :
    formal_part -> formal_expr_val -> formal_expr_val **)

let rec coq_Mul_one_transfer s1 = function
| [] -> []
| a :: s' -> (coq_Mul_one_one s1 a) :: (coq_Mul_one_transfer s1 s')

(** val coq_Mul_transfer :
    formal_expr_val -> formal_expr_val -> formal_expr_val **)

let rec coq_Mul_transfer s1 s2 =
  match s1 with
  | [] -> []
  | a :: s' ->
    coq_Add_transfer (coq_Mul_one_transfer a s2) (coq_Mul_transfer s' s2)

(** val coq_Transfer : expr_val -> formal_expr_val **)

let rec coq_Transfer a = match a with
| Ez_val a' -> ([], a') :: []
| Vuop (u, a0) ->
  (match u with
   | Oneg -> coq_Neg_transfer (coq_Transfer a0)
   | _ -> ((a :: []), (Zpos Coq_xH)) :: [])
| Vbop (b0, a0, b) ->
  (match b0 with
   | Oadd -> coq_Add_transfer (coq_Transfer a0) (coq_Transfer b)
   | Osub -> coq_Sub_transfer (coq_Transfer a0) (coq_Transfer b)
   | Omul -> coq_Mul_transfer (coq_Transfer a0) (coq_Transfer b)
   | _ -> ((a :: []), (Zpos Coq_xH)) :: [])
| _ -> ((a :: []), (Zpos Coq_xH)) :: []

(** val coq_Transfer_expr_val : expr_val -> formal_expr_val **)

let coq_Transfer_expr_val a =
  coq_Transfer (coq_Simplify_val a)
