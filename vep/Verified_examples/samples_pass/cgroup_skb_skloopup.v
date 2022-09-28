From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop Ebpf_helper.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op7 (ALU64 BPF_MOV) BPF_X 7 1 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 6 0 0 1 ::
	Op1 (LDX BPF_W BPF_MEM) 1 7 16 0 ::
	Op5 (JMP BPF_JNE) BPF_K 1 0 18 56710 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 (-40) ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 7 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 4 0 0 40 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 26 ::
	Op5 (JMP BPF_JNE) BPF_K 0 0 11 0 ::
	Op1 (LDX BPF_B BPF_MEM) 1 10 (-34) 0 ::
	Op5 (JMP BPF_JNE) BPF_K 1 0 9 6 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 (-64) ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 7 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 40 ::
	Op7 (ALU64 BPF_MOV) BPF_K 4 0 0 20 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 26 ::
	Op7 (ALU64 BPF_MOV) BPF_K 6 0 0 1 ::
	Op5 (JMP BPF_JNE) BPF_K 0 0 1 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 6 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 0 6 0 0 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.


Definition Real_tag := (Anormal nil Ebpf_Local_init (Other _ _ (Ebpf_context x (Ez_val 76) (Ez_val 80) packet_list) :: Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.  

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.
  
Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

Definition Result := Program_exec Real_tag STATEMENT.