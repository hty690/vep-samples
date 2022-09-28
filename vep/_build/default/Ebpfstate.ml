open BinInt
open BinNums
open Datatypes
open Localdef
open Map_sep
open Sepdef
open Exprdef
open List_lemma

(** val coq_R0 : ident **)

let coq_R0 =
  Coq_xH

(** val r0 : expr_val **)

let r0 =
  V_vari coq_R0

(** val coq_R1 : ident **)

let coq_R1 =
  Coq_xO Coq_xH

(** val r1 : expr_val **)

let r1 =
  V_vari coq_R1

(** val coq_R2 : ident **)

let coq_R2 =
  Coq_xI Coq_xH

(** val r2 : expr_val **)

let r2 =
  V_vari coq_R2

(** val coq_R3 : ident **)

let coq_R3 =
  Coq_xO (Coq_xO Coq_xH)

(** val r3 : expr_val **)

let r3 =
  V_vari coq_R3

(** val coq_R4 : ident **)

let coq_R4 =
  Coq_xI (Coq_xO Coq_xH)

(** val r4 : expr_val **)

let r4 =
  V_vari coq_R4

(** val coq_R5 : ident **)

let coq_R5 =
  Coq_xO (Coq_xI Coq_xH)

(** val r5 : expr_val **)

let r5 =
  V_vari coq_R5

(** val coq_R6 : ident **)

let coq_R6 =
  Coq_xI (Coq_xI Coq_xH)

(** val r6 : expr_val **)

let r6 =
  V_vari coq_R6

(** val coq_R7 : ident **)

let coq_R7 =
  Coq_xO (Coq_xO (Coq_xO Coq_xH))

(** val r7 : expr_val **)

let r7 =
  V_vari coq_R7

(** val coq_R8 : ident **)

let coq_R8 =
  Coq_xI (Coq_xO (Coq_xO Coq_xH))

(** val r8 : expr_val **)

let r8 =
  V_vari coq_R8

(** val coq_R9 : ident **)

let coq_R9 =
  Coq_xO (Coq_xI (Coq_xO Coq_xH))

(** val r9 : expr_val **)

let r9 =
  V_vari coq_R9

(** val coq_R10 : ident **)

let coq_R10 =
  Coq_xI (Coq_xI (Coq_xO Coq_xH))

(** val r10 : expr_val **)

let r10 =
  V_vari coq_R10

(** val coq_Max_size : expr_val **)

let coq_Max_size =
  Ez_val (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))))))

(** val nullptr : expr_val **)

let nullptr =
  Ez_val Z0

(** val coq_SV : expr_val **)

let coq_SV =
  Vlist_vari (Coq_xO (Coq_xO (Coq_xI Coq_xH)))

(** val x : expr_val **)

let x =
  V_vari (Coq_xI (Coq_xO (Coq_xI Coq_xH)))

(** val data_ed : expr_val **)

let data_ed =
  V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))

(** val coq_Context_list : expr_val **)

let coq_Context_list =
  Vlist_vari (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))

(** val coq_Ebpf_Local_init : coq_Local list **)

let coq_Ebpf_Local_init =
  (Temp (coq_R0, r0)) :: ((Temp (coq_R1, r1)) :: ((Temp (coq_R2,
    r2)) :: ((Temp (coq_R3, r3)) :: ((Temp (coq_R4, r4)) :: ((Temp (coq_R5,
    r5)) :: ((Temp (coq_R6, r6)) :: ((Temp (coq_R7, r7)) :: ((Temp (coq_R8,
    r8)) :: ((Temp (coq_R9, r9)) :: ((Temp (coq_R10, r10)) :: []))))))))))

(** val coq_Ebpf_Sep_init : coq_Ebpf_Sep list **)

let coq_Ebpf_Sep_init =
  (Data_at (nullptr, SCALAR_VALUE, r0)) :: ((Data_at (x, PTR_TO_CTX,
    r1)) :: ((Data_at (nullptr, NOT_INIT, r2)) :: ((Data_at (nullptr,
    NOT_INIT, r3)) :: ((Data_at (nullptr, NOT_INIT, r4)) :: ((Data_at
    (nullptr, NOT_INIT, r5)) :: ((Data_at (nullptr, NOT_INIT,
    r6)) :: ((Data_at (nullptr, NOT_INIT, r7)) :: ((Data_at (nullptr,
    NOT_INIT, r8)) :: ((Data_at (nullptr, NOT_INIT, r9)) :: ((Memory (r10,
    coq_Max_size, coq_SV)) :: []))))))))))

(** val inf : nat **)

let inf =
  Z.to_nat (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))))))))))
