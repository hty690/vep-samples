From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
  Op7 (ALU64 BPF_MOV) BPF_X 1 10 0 0 ::
  Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 (-1) ::
  Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 20 ::
  Op5 (BPF_CALL) BPF_K 0 0 0 16 ::
  Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition Real_tag := ((Anormal nil Ebpf_Local_init Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.

Definition Virtual_tag := Eval_from_Real len Real_tag.

Definition After_exec := Program_exec_several Virtual_tag STATEMENT Real_tag len.

Compute (No_error_all After_exec).
