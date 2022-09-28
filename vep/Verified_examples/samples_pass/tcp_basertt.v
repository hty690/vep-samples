From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop Ebpf_helper.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op7 (ALU64 BPF_MOV) BPF_X 6 1 0 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 4294967295 ::
	Empty_line  ::
	Op1 (LDX BPF_W BPF_MEM) 2 6 20 0 ::
	Op5 (JMP BPF_JNE) BPF_K 2 0 36 10 ::
	Op1 (LDX BPF_W BPF_MEM) 2 6 32 0 ::
	Op1 (LDX BPF_W BPF_MEM) 3 6 48 0 ::
	Op5 (JMP BPF_JNE) BPF_X 3 2 33 0 ::
	Op1 (LDX BPF_W BPF_MEM) 2 6 52 0 ::
	Op1 (LDX BPF_W BPF_MEM) 3 6 36 0 ::
	Op7 (ALU64 BPF_XOR) BPF_X 3 2 0 0 ::
	Op4 (ALU BPF_END) BPF_X 3 0 0 32 ::
	Op5 (JMP BPF_JGT) BPF_K 3 0 28 1048575 ::
	Op1 (LDX BPF_W BPF_MEM) 2 6 0 0 ::
	Op5 (JMP BPF_JNE) BPF_K 2 0 26 7 ::
	Op7 (ALU64 BPF_MOV) BPF_X 4 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 4 0 0 (-20) ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 6 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 6 ::
	Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 13 ::
	Op7 (ALU64 BPF_MOV) BPF_K 5 0 0 20 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 57 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 0 0 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 1 0 0 32 ::
	Op7 (ALU64 BPF_RSH) BPF_K 1 0 0 32 ::
	Op5 (JMP BPF_JNE) BPF_K 1 0 14 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 10 (-19) 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 1 0 0 8 ::
	Op1 (LDX BPF_B BPF_MEM) 2 10 (-20) 0 ::
	Op7 (ALU64 BPF_OR) BPF_X 1 2 0 0 ::
	Op1 (LDX BPF_B BPF_MEM) 2 10 (-17) 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 2 0 0 8 ::
	Op1 (LDX BPF_B BPF_MEM) 3 10 (-18) 0 ::
	Op7 (ALU64 BPF_OR) BPF_X 2 3 0 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 2 0 0 16 ::
	Op7 (ALU64 BPF_OR) BPF_X 2 1 0 0 ::
	Op0 (LD BPF_DW BPF_IMM) 0 0 0 4294967295 ::
	Empty_line  ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 80 ::
	Op5 (JMP BPF_JEQ) BPF_K 2 0 1 30318 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 0 0 0 ::
	Op3 (STX BPF_W BPF_MEM) 6 1 4 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 1 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition Real_tag := (Anormal nil Ebpf_Local_init (Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.  

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.

Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

Definition Result := Program_exec Real_tag STATEMENT.