From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop Ebpf_helper.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op7 (ALU64 BPF_MOV) BPF_X 6 1 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 1 ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 4 0 ::
	Op1 (LDX BPF_W BPF_MEM) 2 6 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 2 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 14 ::
	Op5 (JMP BPF_JGT) BPF_X 3 1 104 0 ::
	Op1 (LDX BPF_B BPF_MEM) 3 2 12 0 ::
	Op1 (LDX BPF_B BPF_MEM) 4 2 13 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 4 0 0 8 ::
	Op7 (ALU64 BPF_OR) BPF_X 4 3 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 2 ::
	Op5 (JMP BPF_JNE) BPF_K 4 0 98 8 ::
	Op7 (ALU64 BPF_SUB) BPF_X 1 2 0 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 1 0 0 32 ::
	Op7 (ALU64 BPF_ARSH) BPF_K 1 0 0 32 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 601 ::
	Op5 (JMP BPF_JSGT) BPF_X 2 1 93 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 98 ::
	Op7 (ALU64 BPF_SUB) BPF_X 2 1 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 6 0 0 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 65 ::
	Op5 (JMP BPF_JNE) BPF_K 0 0 88 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 6 0 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 0 0 4294967268 ::
	Empty_line  ::
	Op5 (BPF_CALL) BPF_K 0 0 0 44 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 1 ::
	Op5 (JMP BPF_JNE) BPF_K 0 0 82 0 ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 4 0 ::
	Op1 (LDX BPF_W BPF_MEM) 6 6 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 2 6 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 126 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 1 ::
	Op5 (JMP BPF_JGT) BPF_X 2 1 76 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 33 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 11 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 32 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 10 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 31 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 9 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 30 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 8 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 29 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 7 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 28 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 6 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 37 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 3 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 36 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 2 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 35 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 1 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 34 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 0 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 39 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 5 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 38 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 4 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 41 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 13 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 6 40 0 ::
	Op3 (STX BPF_B BPF_MEM) 6 1 12 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 1027 ::
	Op3 (STX BPF_W BPF_MEM) 6 1 34 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 18946 ::
	Op3 (STX BPF_HW BPF_MEM) 6 1 40 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 6 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 34 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 4 0 0 92 ::
	Op7 (ALU64 BPF_MOV) BPF_K 5 0 0 0 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 28 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 320 ::
	Op3 (STX BPF_W BPF_MEM) 6 1 22 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 1879048261 ::
	Op3 (STX BPF_W BPF_MEM) 6 1 14 0 ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 54 0 ::
	Op3 (STX BPF_W BPF_MEM) 6 1 30 0 ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 58 0 ::
	Op3 (STX BPF_W BPF_MEM) 6 1 26 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 0 0 0 ::
	Op7 (ALU64 BPF_RSH) BPF_K 1 0 0 16 ::
	Op7 (ALU64 BPF_ADD) BPF_X 1 0 0 0 ::
	Op7 (ALU64 BPF_XOR) BPF_K 1 0 0 (-1) ::
	Op3 (STX BPF_HW BPF_MEM) 6 1 36 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 6 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 14 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 4 0 0 20 ::
	Op7 (ALU64 BPF_MOV) BPF_K 5 0 0 0 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 28 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 0 0 0 ::
	Op7 (ALU64 BPF_RSH) BPF_K 1 0 0 16 ::
	Op7 (ALU64 BPF_ADD) BPF_X 1 0 0 0 ::
	Op7 (ALU64 BPF_XOR) BPF_K 1 0 0 (-1) ::
	Op3 (STX BPF_HW BPF_MEM) 6 1 24 0 ::
	Op3 (STX BPF_DW BPF_MEM) 10 7 (-8) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 2 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 (-8) ::
	Op0 (LD BPF_DW BPF_IMM) 1 1 0 0 ::
	Empty_line  ::
	Op5 (BPF_CALL) BPF_K 0 0 0 1 ::
	Op5 (JMP BPF_JEQ) BPF_K 0 0 3 0 ::
	Op1 (LDX BPF_DW BPF_MEM) 1 0 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 1 ::
	Op3 (STX BPF_DW BPF_MEM) 0 1 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 7 0 0 3 ::
	Op7 (ALU64 BPF_MOV) BPF_X 0 7 0 0 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition Real_tag := (Anormal nil Ebpf_Local_init (Other _ _ (Ebpf_context x (Ez_val 0) (Ez_val 4) packet_list) :: Other _ _ (Ebpf_map (Ez_val 0) (Ez_val 2) (Ez_val 4) (Ez_val 8) (Ez_val 1)) :: Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := 70%nat.  

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.

Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

Definition Result := Program_exec Real_tag STATEMENT.