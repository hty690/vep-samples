open BinNat
open BinNums
open BinPos
open Datatypes

module Z =
 struct
  (** val double : coq_Z -> coq_Z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (Coq_xO p)
  | Zneg p -> Zneg (Coq_xO p)

  (** val succ_double : coq_Z -> coq_Z **)

  let succ_double = function
  | Z0 -> Zpos Coq_xH
  | Zpos p -> Zpos (Coq_xI p)
  | Zneg p -> Zneg (Pos.pred_double p)

  (** val pred_double : coq_Z -> coq_Z **)

  let pred_double = function
  | Z0 -> Zneg Coq_xH
  | Zpos p -> Zpos (Pos.pred_double p)
  | Zneg p -> Zneg (Coq_xI p)

  (** val pos_sub : positive -> positive -> coq_Z **)

  let rec pos_sub x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> double (pos_sub p q)
       | Coq_xO q -> succ_double (pos_sub p q)
       | Coq_xH -> Zpos (Coq_xO p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> pred_double (pos_sub p q)
       | Coq_xO q -> double (pos_sub p q)
       | Coq_xH -> Zpos (Pos.pred_double p))
    | Coq_xH ->
      (match y with
       | Coq_xI q -> Zneg (Coq_xO q)
       | Coq_xO q -> Zneg (Pos.pred_double q)
       | Coq_xH -> Z0)

  (** val add : coq_Z -> coq_Z -> coq_Z **)

  let add x y =
    match x with
    | Z0 -> y
    | Zpos x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> Zpos (Pos.add x' y')
       | Zneg y' -> pos_sub x' y')
    | Zneg x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> pos_sub y' x'
       | Zneg y' -> Zneg (Pos.add x' y'))

  (** val opp : coq_Z -> coq_Z **)

  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0

  (** val sub : coq_Z -> coq_Z -> coq_Z **)

  let sub m n =
    add m (opp n)

  (** val mul : coq_Z -> coq_Z -> coq_Z **)

  let mul x y =
    match x with
    | Z0 -> Z0
    | Zpos x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zpos (Pos.mul x' y')
       | Zneg y' -> Zneg (Pos.mul x' y'))
    | Zneg x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zneg (Pos.mul x' y')
       | Zneg y' -> Zpos (Pos.mul x' y'))

  (** val pow_pos : coq_Z -> positive -> coq_Z **)

  let pow_pos z =
    Pos.iter (mul z) (Zpos Coq_xH)

  (** val pow : coq_Z -> coq_Z -> coq_Z **)

  let pow x = function
  | Z0 -> Zpos Coq_xH
  | Zpos p -> pow_pos x p
  | Zneg _ -> Z0

  (** val compare : coq_Z -> coq_Z -> comparison **)

  let compare x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Eq
             | Zpos _ -> Lt
             | Zneg _ -> Gt)
    | Zpos x' -> (match y with
                  | Zpos y' -> Pos.compare x' y'
                  | _ -> Gt)
    | Zneg x' ->
      (match y with
       | Zneg y' -> coq_CompOpp (Pos.compare x' y')
       | _ -> Lt)

  (** val leb : coq_Z -> coq_Z -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : coq_Z -> coq_Z -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val eqb : coq_Z -> coq_Z -> bool **)

  let eqb x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> true
             | _ -> false)
    | Zpos p -> (match y with
                 | Zpos q -> Pos.eqb p q
                 | _ -> false)
    | Zneg p -> (match y with
                 | Zneg q -> Pos.eqb p q
                 | _ -> false)

  (** val to_nat : coq_Z -> nat **)

  let to_nat = function
  | Zpos p -> Pos.to_nat p
  | _ -> O

  (** val of_nat : nat -> coq_Z **)

  let of_nat = function
  | O -> Z0
  | S n0 -> Zpos (Pos.of_succ_nat n0)

  (** val of_N : coq_N -> coq_Z **)

  let of_N = function
  | N0 -> Z0
  | Npos p -> Zpos p

  (** val to_pos : coq_Z -> positive **)

  let to_pos = function
  | Zpos p -> p
  | _ -> Coq_xH

  (** val pos_div_eucl : positive -> coq_Z -> coq_Z * coq_Z **)

  let rec pos_div_eucl a b =
    match a with
    | Coq_xI a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = add (mul (Zpos (Coq_xO Coq_xH)) r) (Zpos Coq_xH) in
      if ltb r' b
      then ((mul (Zpos (Coq_xO Coq_xH)) q), r')
      else ((add (mul (Zpos (Coq_xO Coq_xH)) q) (Zpos Coq_xH)), (sub r' b))
    | Coq_xO a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = mul (Zpos (Coq_xO Coq_xH)) r in
      if ltb r' b
      then ((mul (Zpos (Coq_xO Coq_xH)) q), r')
      else ((add (mul (Zpos (Coq_xO Coq_xH)) q) (Zpos Coq_xH)), (sub r' b))
    | Coq_xH ->
      if leb (Zpos (Coq_xO Coq_xH)) b
      then (Z0, (Zpos Coq_xH))
      else ((Zpos Coq_xH), Z0)

  (** val div_eucl : coq_Z -> coq_Z -> coq_Z * coq_Z **)

  let div_eucl a b =
    match a with
    | Z0 -> (Z0, Z0)
    | Zpos a' ->
      (match b with
       | Z0 -> (Z0, a)
       | Zpos _ -> pos_div_eucl a' b
       | Zneg b' ->
         let (q, r) = pos_div_eucl a' (Zpos b') in
         (match r with
          | Z0 -> ((opp q), Z0)
          | _ -> ((opp (add q (Zpos Coq_xH))), (add b r))))
    | Zneg a' ->
      (match b with
       | Z0 -> (Z0, a)
       | Zpos _ ->
         let (q, r) = pos_div_eucl a' b in
         (match r with
          | Z0 -> ((opp q), Z0)
          | _ -> ((opp (add q (Zpos Coq_xH))), (sub b r)))
       | Zneg b' -> let (q, r) = pos_div_eucl a' (Zpos b') in (q, (opp r)))

  (** val div : coq_Z -> coq_Z -> coq_Z **)

  let div a b =
    let (q, _) = div_eucl a b in q

  (** val modulo : coq_Z -> coq_Z -> coq_Z **)

  let modulo a b =
    let (_, r) = div_eucl a b in r

  (** val coq_land : coq_Z -> coq_Z -> coq_Z **)

  let coq_land a b =
    match a with
    | Z0 -> Z0
    | Zpos a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (Pos.coq_land a0 b0)
       | Zneg b0 -> of_N (N.ldiff (Npos a0) (Pos.pred_N b0)))
    | Zneg a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (N.ldiff (Npos b0) (Pos.pred_N a0))
       | Zneg b0 ->
         Zneg (N.succ_pos (N.coq_lor (Pos.pred_N a0) (Pos.pred_N b0))))
 end
