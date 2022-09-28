From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition Manacher_STATEMENT : list Singleton_statement := 
  Op1 (LDX BPF_W BPF_MEM) 2 1 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 35 ::
Op3 (STX BPF_B BPF_MEM) 10 3 (-383) 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 36 ::
Op3 (STX BPF_B BPF_MEM) 10 1 (-384) 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 2 ::
Op5 (JMP BPF_JEQ) BPF_K 2 0 19 0 ::
Op3 (STX BPF_B BPF_MEM) 10 3 (-381) 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 4 ::
Op5 (JMP BPF_JEQ) BPF_K 2 0 16 1 ::
Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 1 ::
Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 4 0 0 35 ::
Op7 (ALU64 BPF_MOV) BPF_X 5 10 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 5 0 0 (-384) ::
Op7 (ALU64 BPF_ADD) BPF_X 5 1 0 0 ::
Op3 (STX BPF_B BPF_MEM) 5 4 5 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 0 10 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 0 0 0 (-20) ::
Op7 (ALU64 BPF_ADD) BPF_X 0 3 0 0 ::
Op1 (LDX BPF_B BPF_MEM) 0 0 0 0 ::
Op3 (STX BPF_B BPF_MEM) 5 0 4 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 2 ::
Op7 (ALU64 BPF_ADD) BPF_K 3 0 0 1 ::
Op5 (JMP BPF_JGT) BPF_X 2 3 (-12) 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 4 ::
Op7 (ALU64 BPF_MOV) BPF_X 2 10 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 (-384) ::
Op7 (ALU64 BPF_MOV) BPF_X 3 2 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_X 3 1 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 4 0 0 63 ::
Op3 (STX BPF_B BPF_MEM) 3 4 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 1 ::
Op7 (ALU64 BPF_MOV) BPF_K 5 0 0 (-1) ::
Op7 (ALU64 BPF_MOV) BPF_K 4 0 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 (-1) ::
Op5 (JMP BPF_GOTO) BPF_K 0 0 16 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 1 ::
Op7 (ALU64 BPF_ADD) BPF_K 4 0 0 1 ::
Op7 (ALU64 BPF_MOV) BPF_X 5 8 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 0 7 0 0 ::
Op5 (JMP BPF_JGT) BPF_X 1 4 11 0 ::
Op0 (LD BPF_DW BPF_IMM) 1 0 0 7216238185367232854 ::
Op3 (STX BPF_DW BPF_MEM) 10 1 (-400) 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 10 ::
Op3 (STX BPF_HW BPF_MEM) 10 1 (-392) 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 1 10 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 (-400) ::
Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 10 ::
Op5 (BPF_CALL) BPF_K 0 0 0 6 ::
Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 2 ::
Op5 (BPF_EXIT) BPF_K 0 0 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_K 6 0 0 1 ::
Op5 (JMP BPF_JSGE) BPF_X 4 0 13 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 7 0 0 0 ::
Op7 (ALU64 BPF_SUB) BPF_X 7 4 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 6 5 0 0 ::
Op7 (ALU64 BPF_LSH) BPF_K 6 0 0 1 ::
Op7 (ALU64 BPF_SUB) BPF_X 6 4 0 0 ::
Op7 (ALU64 BPF_LSH) BPF_K 6 0 0 3 ::
Op7 (ALU64 BPF_MOV) BPF_X 8 10 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 8 0 0 (-344) ::
Op7 (ALU64 BPF_ADD) BPF_X 8 6 0 0 ::
Op1 (LDX BPF_DW BPF_MEM) 6 8 0 0 ::
Op5 (JMP BPF_JSGE) BPF_X 7 6 2 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 7 0 0 1 ::
Op7 (ALU64 BPF_MOV) BPF_X 6 7 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 7 6 0 0 ::
Op7 (ALU64 BPF_NEG) BPF_K 7 0 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 6 0 0 (-1) ::
Op7 (ALU64 BPF_MOV) BPF_X 8 2 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_X 8 7 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 7 0 0 (-1) ::
Op7 (ALU64 BPF_MOV) BPF_X 9 2 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_X 9 6 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 6 0 0 1 ::
Op1 (LDX BPF_B BPF_MEM) 9 9 1 0 ::
Op1 (LDX BPF_B BPF_MEM) 8 8 0 0 ::
Op5 (JMP BPF_JEQ) BPF_X 8 9 (-9) 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 9 4 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_X 9 6 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 7 9 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 7 0 0 (-1) ::
Op5 (JMP BPF_JSGT) BPF_X 9 0 1 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 7 0 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 8 4 0 0 ::
Op5 (JMP BPF_JSGT) BPF_X 9 0 1 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 8 5 0 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 5 4 0 0 ::
Op7 (ALU64 BPF_LSH) BPF_K 5 0 0 3 ::
Op7 (ALU64 BPF_MOV) BPF_X 0 10 0 0 ::
Op7 (ALU64 BPF_ADD) BPF_K 0 0 0 (-344) ::
Op7 (ALU64 BPF_ADD) BPF_X 0 5 0 0 ::
Op3 (STX BPF_DW BPF_MEM) 0 6 0 0 ::
Op5 (JMP BPF_JSGT) BPF_X 3 6 (-59) 0 ::
Op7 (ALU64 BPF_MOV) BPF_X 3 6 0 0 ::
Op5 (JMP BPF_GOTO) BPF_K 0 0 (-61) 0 :: nil.


Definition Real_tag := Ebpf_ass_init.

Definition len := length (Manacher_STATEMENT).

Definition Virtual_tag := Eval_from_Real len Real_tag.

Definition After_exec := Program_exec_several Virtual_tag Manacher_STATEMENT Real_tag len.

Definition Need_to_check := Program_checker Real_tag After_exec.

Definition length_option {A : Type} (a : option (list A)) := match a with | Some c => length c | None => O end.

Compute (length_option (Find_B_in_prodAB Nat.eqb After_exec 96%nat)).
Definition Need' := Program_checker ((Anormal Ebpf_Prop_init Ebpf_Local_init Ebpf_Sep_init , 51%nat) :: nil) After_exec.
Compute (Check_assertion Need').




