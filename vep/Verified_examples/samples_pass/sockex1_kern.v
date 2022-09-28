From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop Ebpf_helper.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op7 (ALU64 BPF_MOV) BPF_X 6 1 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 (-1) ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 23 ::
	Op7 (ALU64 BPF_MOV) BPF_K 4 0 0 1 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 26 ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 4 0 ::
	Op5 (JMP BPF_JNE) BPF_K 1 0 10 4 ::
	Op7 (ALU64 BPF_MOV) BPF_X 2 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 (-1) ::
	Op0 (LD BPF_DW BPF_IMM) 1 1 0 0 ::
	Empty_line  ::
	Op5 (BPF_CALL) BPF_K 0 0 0 1 ::
	Op5 (JMP BPF_JEQ) BPF_K 0 0 4 0 ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 0 0 ::
	Op1 (LDX BPF_DW BPF_MEM) 2 0 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_X 2 1 0 0 ::
	Op3 (STX BPF_DW BPF_MEM) 0 2 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 0 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition Real_tag := (Anormal nil Ebpf_Local_init (Other _ _ (Ebpf_context x (Ez_val 76) (Ez_val 80) packet_list) :: Other _ _ (Ebpf_map (Ez_val 0) (Ez_val 2) (Ez_val 4) (Ez_val 8) (Ez_val 256)) :: Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.  

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.

Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

Definition Result := Program_exec Real_tag STATEMENT.