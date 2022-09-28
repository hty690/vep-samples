From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop Ebpf_helper.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 0 ::
	Op3 (STX BPF_W BPF_MEM) 10 1 (-4) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 1 ::
	Op3 (STX BPF_W BPF_MEM) 10 7 (-8) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 2 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 (-4) ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 0 ::
	Empty_line  ::
	Op5 (BPF_CALL) BPF_K 0 0 0 1 ::
	Op7 (ALU64 BPF_MOV) BPF_X 6 0 0 0 ::
	Op5 (JMP BPF_JEQ) BPF_K 6 0 21 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 2 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 (-8) ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 0 ::
	Empty_line  ::
	Op5 (BPF_CALL) BPF_K 0 0 0 1 ::
	Op5 (JMP BPF_JEQ) BPF_K 0 0 15 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 0 ::
	Op5 (JMP BPF_GOTO) BPF_K 0 0 3 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 1 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 1 ::
	Op5 (JMP BPF_JEQ) BPF_K 1 0 10 200 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 0 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_X 3 1 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 2 6 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_X 2 1 0 0 ::
	Op1 (LDX BPF_B BPF_MEM) 2 2 0 0 ::
	Op1 (LDX BPF_B BPF_MEM) 3 3 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 0 ::
	Op5 (JMP BPF_JNE) BPF_X 2 3 2 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 1 ::
	Op5 (JMP BPF_JNE) BPF_K 2 0 (-13) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 0 7 0 0 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition assertion_22 := Aex 37%positive (Aex 36%positive (Aex 35%positive (Aex 34%positive (Aex 33%positive (Aex 32%positive
  (Anormal
     ((Be Pvlt (V_vari 34%positive) (Ez_val 200)) :: Be Pvle (Ez_val 0) (V_vari 34%positive) :: nil)
        Ebpf_Local_init 
      (Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 4%positive)
      :: Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 5%positive)
         :: Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 6%positive)
            :: Data_at ebpf_type T (V_vari 32%positive) SCALAR_VALUE (V_vari 7%positive)
               :: Data_at ebpf_type T (V_vari 37%positive) SCALAR_VALUE (V_vari 8%positive)
                  :: Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 9%positive)
                     :: Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 10%positive)
                        :: Memory ebpf_type T (V_vari 11%positive) (Ez_val 512) (Vlist_vari 12%positive)
                           :: Data_at ebpf_type T (V_vari 33%positive) SCALAR_VALUE (V_vari 1%positive)
                              :: Data_at ebpf_type T (V_vari 34%positive) SCALAR_VALUE (V_vari 2%positive)
                                 :: Data_at ebpf_type T (V_vari 35%positive) SCALAR_VALUE (V_vari 3%positive)
      :: Other ebpf_type T (Ebpf_map (Ez_val 0) (Ez_val 1) (Ez_val 8) (Ez_val 100) (Ez_val 2))
      :: Other ebpf_type T (Map_element (V_vari 21%positive) (Vlop Vnth (Ez_val (-4)) (Vlist_vari 12%positive)) (Vlist_vari 32%positive) (Ez_val 1000)) 
      :: Other ebpf_type T (Map_element (V_vari 21%positive) (Vlop Vnth (Ez_val (-8)) (Vlist_vari 12%positive)) (Vlist_vari 36%positive) (Ez_val 1000)) :: nil))))))) :: 
      Aex 38%positive (Aex 37%positive (Aex 36%positive (Aex 35%positive (Aex 34%positive (Aex 33%positive (Aex 32%positive
  (Anormal
     ((Be Pvlt (V_vari 34%positive) (Ez_val 200)) :: Be Pvle (Ez_val 0) (V_vari 34%positive) :: nil)
        Ebpf_Local_init 
      (Data_at ebpf_type T (V_vari 38%positive) SCALAR_VALUE (V_vari 4%positive)
      :: Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 5%positive)
         :: Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 6%positive)
            :: Data_at ebpf_type T (V_vari 32%positive) SCALAR_VALUE (V_vari 7%positive)
               :: Data_at ebpf_type T (V_vari 37%positive) SCALAR_VALUE (V_vari 8%positive)
                  :: Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 9%positive)
                     :: Data_at ebpf_type T (Ez_val 0) NOT_INIT (V_vari 10%positive)
                        :: Memory ebpf_type T (V_vari 11%positive) (Ez_val 512) (Vlist_vari 12%positive)
                           :: Data_at ebpf_type T (V_vari 33%positive) SCALAR_VALUE (V_vari 1%positive)
                              :: Data_at ebpf_type T (V_vari 34%positive) SCALAR_VALUE (V_vari 2%positive)
                                 :: Data_at ebpf_type T (V_vari 35%positive) SCALAR_VALUE (V_vari 3%positive)
      :: Other ebpf_type T (Ebpf_map (Ez_val 0) (Ez_val 1) (Ez_val 8) (Ez_val 100) (Ez_val 2))
      :: Other ebpf_type T (Map_element (V_vari 21%positive) (Vlop Vnth (Ez_val (-4)) (Vlist_vari 12%positive)) (Vlist_vari 32%positive) (Ez_val 1000)) 
      :: Other ebpf_type T (Map_element (V_vari 21%positive) (Vlop Vnth (Ez_val (-8)) (Vlist_vari 12%positive)) (Vlist_vari 36%positive) (Ez_val 1000)) :: nil)))))))) :: 
      nil.

Definition Real_tag :=  (assertion_22 , 22%nat) :: (Anormal nil Ebpf_Local_init (Other _ _ (Ebpf_map (Ez_val 0) (Ez_val 1) (Ez_val 8) (Ez_val 1000) (Ez_val 2)) :: Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.  

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.
  
Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

Definition Result := Program_exec Real_tag STATEMENT.