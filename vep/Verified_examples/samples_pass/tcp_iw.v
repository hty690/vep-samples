From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop Ebpf_helper.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op7 (ALU64 BPF_MOV) BPF_X 6 1 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 1500000 ::
	Op3 (STX BPF_W BPF_MEM) 10 1 (-4) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 40 ::
	Op3 (STX BPF_W BPF_MEM) 10 1 (-8) 0 ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 64 0 ::
	Op5 (JMP BPF_JEQ) BPF_K 1 0 4 836304896 ::
	Op0 (LD BPF_DW BPF_IMM) 0 0 0 4294967295 ::
	Empty_line  ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 68 0 ::
	Op5 (JMP BPF_JNE) BPF_K 1 0 35 55601 ::
	Op1 (LDX BPF_W BPF_MEM) 1 6 0 0 ::
	Op5 (JMP BPF_JSGT) BPF_K 1 0 21 3 ::
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 40 ::
	Op5 (JMP BPF_JEQ) BPF_K 1 0 31 2 ::
	Op5 (JMP BPF_JEQ) BPF_K 1 0 1 3 ::
	Op5 (JMP BPF_GOTO) BPF_K 0 0 19 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 7 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 7 0 0 (-4) ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 6 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 1 ::
	Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 7 ::
	Op7 (ALU64 BPF_MOV) BPF_X 4 7 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 5 0 0 4 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 49 ::
	Op7 (ALU64 BPF_MOV) BPF_X 8 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 6 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 1 ::
	Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 8 ::
	Op7 (ALU64 BPF_MOV) BPF_X 4 7 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 5 0 0 4 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 49 ::
	Op7 (ALU64 BPF_ADD) BPF_X 0 8 0 0 ::
	Op5 (JMP BPF_GOTO) BPF_K 0 0 12 0 ::
	Op5 (JMP BPF_JEQ) BPF_K 1 0 4 4 ::
	Op5 (JMP BPF_JEQ) BPF_K 1 0 (-19) 5 ::
	Op0 (LD BPF_DW BPF_IMM) 0 0 0 4294967295 ::
	Empty_line  ::
	Op5 (JMP BPF_GOTO) BPF_K 0 0 7 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 4 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 4 0 0 (-8) ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 6 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 6 ::
	Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 1001 ::
	Op7 (ALU64 BPF_MOV) BPF_K 5 0 0 4 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 49 ::
	Op3 (STX BPF_W BPF_MEM) 6 0 4 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 1 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition Real_tag := (Anormal nil Ebpf_Local_init (Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.  

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.

Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

Definition Result := Program_exec Real_tag STATEMENT.
