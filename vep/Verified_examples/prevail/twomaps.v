From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op5 (BPF_CALL) BPF_K 0 0 0 7 ::
	Op7 (ALU64 BPF_AND) BPF_K 0 0 0 1 ::
	Op0 (LD BPF_DW BPF_IMM) 1 1 0 1 ::
	Empty_line ::
	Op5 (JMP BPF_JEQ) BPF_K 0 0 2 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 1 0 0 ::
	Empty_line  ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 10 ::
	Op3 (STX BPF_W BPF_MEM) 10 2 (-4) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 2 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 (-4) ::
	Op5 (BPF_CALL) BPF_K 0 0 0 1 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 0 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 0 ::
	Op5 (JMP BPF_JEQ) BPF_K 1 0 1 0 ::
	Op1 (LDX BPF_DW BPF_MEM) 0 1 0 0 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.


Definition Real_tag := (Anormal nil Ebpf_Local_init ((Other ebpf_type Ebpf_t (Ebpf_map (Ez_val 0) (Ez_val 2) (Ez_val 4) (Ez_val 8) (Ez_val 1))) :: (Other ebpf_type Ebpf_t (Ebpf_map (Ez_val 1) (Ez_val 2) (Ez_val 4) (Ez_val 4) (Ez_val 2))) :: Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.

Definition Virtual_tag := Eval_from_Real len Real_tag.

Definition After_exec := Program_exec_several Virtual_tag STATEMENT Real_tag len.

(* Compute (Check_assertion Need_to_check).

Compute (No_error_all After_exec). *)