(* This file contain the assertion of Ebpf programs states*)
From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstatement.
From Ebpf Require Export Map_sep.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

(** Ebpf contain R0 - R10  64bits registers **)

Definition R0 : ident := 1%positive.
Definition r0 := (V_vari R0).
(* return value from in-kernel function, and exit value for eBPF program *)

Definition R1 : ident := 2%positive.
Definition r1 := (V_vari R1).
Definition R2 : ident := 3%positive.
Definition r2 := (V_vari R2).
Definition R3 : ident := 4%positive.
Definition r3 := (V_vari R3).
Definition R4 : ident := 5%positive.
Definition r4 := (V_vari R4).
Definition R5 : ident := 6%positive.
Definition r5 := (V_vari R5).
(* arguments from eBPF program to in-kernel function *)

Definition R6 : ident := 7%positive.
Definition r6 := (V_vari R6).
Definition R7 : ident := 8%positive.
Definition r7 := (V_vari R7).
Definition R8 : ident := 9%positive.
Definition r8 := (V_vari R8).
Definition R9 : ident := 10%positive.
Definition r9 := (V_vari R9).
(* callee saved registers that in-kernel function will preserve *)

Definition R10 : ident := 11%positive.
Definition r10 := (V_vari R10).
Definition Max_size := (Ez_val 512).
(* read-only frame pointer to access stack , stack size 512byte *)

Definition nullptr := (Ez_val 0).

Definition SV := (Vlist_vari 12%positive). (* stack *)

Definition x := (V_vari 13%positive).
Definition i := (V_vari 14%positive).

Definition packet_list := (Vlist_vari 15%positive).
Definition data_st := (V_vari 16%positive).
Definition data_ed := (V_vari 17%positive).
Definition Context_list := (Vlist_vari 18%positive).

Definition ident_used := 20%positive.

Definition Ebpf_Prop_init : list Proposition := nil.

Definition Ebpf_Local_init : list Local := 
  Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 ::
  Temp R4 r4 :: Temp R5 r5 :: Temp R6 r6 :: Temp R7 r7 ::
  Temp R8 r8 :: Temp R9 r9 :: Temp R10 r10 :: nil. 

Definition Ebpf_Sep_init : list Ebpf_Sep := 
  Data_at _ _ nullptr SCALAR_VALUE r0 :: Data_at _ _ x PTR_TO_CTX r1 :: Data_at _ _ nullptr NOT_INIT r2 :: Data_at _ _ nullptr NOT_INIT r3 :: Data_at _ _ nullptr NOT_INIT r4 :: Data_at _ _ nullptr NOT_INIT r5 :: Data_at _ _ nullptr NOT_INIT r6 :: Data_at _ _ nullptr NOT_INIT r7 :: Data_at _ _ nullptr NOT_INIT r8 :: Data_at _ _ nullptr NOT_INIT r9 :: Memory _ _ r10 Max_size SV :: nil.

Definition Ebpf_ass_init : list (list assertion * nat) := 
  (Anormal Ebpf_Prop_init Ebpf_Local_init Ebpf_Sep_init :: nil, O) :: nil.

Definition inf : nat := Z.to_nat 10000.
