From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.


Definition KMP_STATEMENT : list Singleton_statement := 
  Op1 (LDX BPF_W BPF_MEM) 1 1 0 0 ::
  Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 (-1) ::
  Op3 (STX BPF_DW BPF_MEM) 10 2 (-456) 0 ::
  Op5 (JMP BPF_JEQ) BPF_K 1 0 26 0 ::
  Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 0 ::
  Op5 (JMP BPF_JEQ) BPF_K 2 0 8 (-1) ::
  Op7 (ALU64 BPF_MOV) BPF_X 4 10 0 0 ::
  Op7 (ALU64 BPF_ADD) BPF_K 4 0 0 (-50) ::
  Op7 (ALU64 BPF_MOV) BPF_X 5 4 0 0 ::
  Op7 (ALU64 BPF_ADD) BPF_X 5 3 0 0 ::
  Op7 (ALU64 BPF_ADD) BPF_X 4 2 0 0 ::
  Op1 (LDX BPF_B BPF_MEM) 4 4 0 0 ::
  Op1 (LDX BPF_B BPF_MEM) 5 5 0 0 ::
  Op5 (JMP BPF_JNE) BPF_X 5 4 10 0 ::
  Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 1 ::
  Op7 (ALU64 BPF_MOV) BPF_X 4 3 0 0 ::
  Op7 (ALU64 BPF_LSH) BPF_K 4 0 0 3 ::
  Op7 (ALU64 BPF_MOV) BPF_X 5 10 0 0 ::
  Op7 (ALU64 BPF_ADD) BPF_K 5 0 0 (-456) ::
  Op7 (ALU64 BPF_ADD) BPF_X 5 4 0 0 :: 
  Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 1 ::
  Op3 (STX BPF_DW BPF_MEM) 5 2 0 0 :: 
  Op5 (JMP BPF_JSGT) BPF_X 1 3 (-18) 0 ::
  Op5 (JMP BPF_GOTO) BPF_K 0 0 6 0 :: 
  Op7 (ALU64 BPF_LSH) BPF_K 2 0 0 3 :: 
  Op7 (ALU64 BPF_MOV) BPF_X 4 10 0 0 ::
  Op7 (ALU64 BPF_ADD) BPF_K 4 0 0 (-456) ::
  Op7 (ALU64 BPF_ADD) BPF_X 4 2 0 0 :: 
  Op1 (LDX BPF_DW BPF_MEM) 2 4 0 0 ::
  Op5 (JMP BPF_JSGT) BPF_X 1 3 (-25) 0 ::
  Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 2 ::
  Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition assertion_5 :=
  Aex 21%positive (Aex 22%positive (Aex 23%positive (Aex 24%positive
  (Anormal 
  (Up Pnot (Be Pvequal x (Ez_val 0)) :: (Qf PForall i (Bp Pimply (Bp Pand (Be Pvle (Ez_val 0%Z) i) (Be Pvle i (V_vari 21%positive))) (Bp Pand (Be Pvle (Ez_val (-1)) (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV)) (Be Pvle (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV) i) )) ) :: (Be Pvlt (V_vari 21%positive) x) :: nil)
    Ebpf_Local_init 
  (Data_at _ _ nullptr SCALAR_VALUE r0 :: Data_at _ _ x SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r5 :: Data_at _ _ nullptr NOT_INIT r6 :: Data_at _ _ nullptr NOT_INIT r7 :: Data_at _ _ nullptr NOT_INIT r8 :: Data_at _ _ nullptr NOT_INIT r9 :: Memory _ _ r10 Max_size SV :: nil)
  )))) :: nil.

Definition assertion_14 :=
  Aex 21%positive (Aex 22%positive (Aex 23%positive (Aex 24%positive
  (Anormal 
  (Up Pnot (Be Pvequal x (Ez_val 0)) :: (Qf PForall i (Bp Pimply (Bp Pand (Be Pvle (Ez_val 0%Z) i) (Be Pvle i (V_vari 21%positive))) (Bp Pand (Be Pvle (Ez_val (-1)) (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV)) (Be Pvle (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV) i) )) ) :: nil)
    Ebpf_Local_init 
  (Data_at _ _ nullptr SCALAR_VALUE r0 :: Data_at _ _ x SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r5 :: Data_at _ _ nullptr NOT_INIT r6 :: Data_at _ _ nullptr NOT_INIT r7 :: Data_at _ _ nullptr NOT_INIT r8 :: Data_at _ _ nullptr NOT_INIT r9 :: Memory _ _ r10 Max_size SV :: nil)
  )))) :: nil.

Definition assertion_22 :=
  Aex 21%positive (Aex 22%positive (Aex 23%positive (Aex 24%positive
  (Anormal 
  (Up Pnot (Be Pvequal x (Ez_val 0)) :: (Qf PForall i (Bp Pimply (Bp Pand (Be Pvle (Ez_val 0%Z) i) (Be Pvle i (V_vari 21%positive))) (Bp Pand (Be Pvle (Ez_val (-1)) (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV)) (Be Pvle (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV) i) )) ) :: nil)
    Ebpf_Local_init 
  (Data_at _ _ nullptr SCALAR_VALUE r0 :: Data_at _ _ x SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r5 :: Data_at _ _ nullptr NOT_INIT r6 :: Data_at _ _ nullptr NOT_INIT r7 :: Data_at _ _ nullptr NOT_INIT r8 :: Data_at _ _ nullptr NOT_INIT r9 :: Memory _ _ r10 Max_size SV :: nil)
  )))) :: nil.

Definition assertion_24 :=
  Aex 21%positive (Aex 22%positive (Aex 23%positive (Aex 24%positive
  (Anormal 
  (Up Pnot (Be Pvequal x (Ez_val 0)) :: (Qf PForall i (Bp Pimply (Bp Pand (Be Pvle (Ez_val 0%Z) i) (Be Pvle i (V_vari 21%positive))) (Bp Pand (Be Pvle (Ez_val (-1)) (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV)) (Be Pvle (Vlop Vnth (Vbop Oadd (Ez_val (-456)) (Vbop Oshl i (Ez_val 3))) SV) i) )) ) :: nil)
    Ebpf_Local_init 
  (Data_at _ _ nullptr SCALAR_VALUE r0 :: Data_at _ _ x SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r5 :: Data_at _ _ nullptr NOT_INIT r6 :: Data_at _ _ nullptr NOT_INIT r7 :: Data_at _ _ nullptr NOT_INIT r8 :: Data_at _ _ nullptr NOT_INIT r9 :: Memory _ _ r10 Max_size SV :: nil)
  )))) :: nil.

Definition Real_tag := (assertion_24, 24%nat) :: (assertion_22 , 22%nat) :: (assertion_14, 14%nat) :: (assertion_5 , 5%nat) :: Ebpf_ass_init.

Definition len := length (KMP_STATEMENT).

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) KMP_STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.
  
Definition Result := Program_exec Real_tag KMP_STATEMENT. 

Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

