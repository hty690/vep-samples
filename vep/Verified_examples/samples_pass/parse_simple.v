From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop Ebpf_helper.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 0 ::
	Op1 (LDX BPF_W BPF_MEM) 2 1 80 0 ::
	Op1 (LDX BPF_W BPF_MEM) 1 1 76 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 1 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 42 ::
	Op5 (JMP BPF_JGT) BPF_X 3 2 15 0 ::
	Op1 (LDX BPF_HW BPF_MEM) 2 1 12 0 ::
	Op5 (JMP BPF_JNE) BPF_K 2 0 13 8 ::
	Op1 (LDX BPF_B BPF_MEM) 2 1 23 0 ::
	Op5 (JMP BPF_JNE) BPF_K 2 0 11 17 ::
	Op1 (LDX BPF_B BPF_MEM) 2 1 14 0 ::
	Op7 (ALU64 BPF_AND) BPF_K 2 0 0 15 ::
	Op5 (JMP BPF_JNE) BPF_K 2 0 8 5 ::
	Op1 (LDX BPF_HW BPF_MEM) 2 1 20 0 ::
	Op7 (ALU64 BPF_AND) BPF_K 2 0 0 65343 ::
	Op5 (JMP BPF_JNE) BPF_K 2 0 5 0 ::
	Op1 (LDX BPF_HW BPF_MEM) 1 1 36 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 1 ::
	Op5 (JMP BPF_JEQ) BPF_K 1 0 1 2304 ::
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 0 0 0 1 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition Real_tag := (Anormal nil Ebpf_Local_init (Other _ _ (Ebpf_context x (Ez_val 76) (Ez_val 80) packet_list):: Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.  

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.

Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

Definition Result := Program_exec Real_tag STATEMENT.