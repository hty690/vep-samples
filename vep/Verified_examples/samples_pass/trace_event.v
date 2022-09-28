From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstate EbpfExec Ebpfstatement Ebpfop Ebpf_helper.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition STATEMENT : list Singleton_statement := 
	Op7 (ALU64 BPF_MOV) BPF_X 6 1 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 30060 ::
	Op3 (STX BPF_HW BPF_MEM) 10 1 (-4) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 1814372410 ::
	Op3 (STX BPF_W BPF_MEM) 10 1 (-8) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 7453010356365251104 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 1 (-16) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 7308613580330136940 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 1 (-24) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 7792670165399071842 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 1 (-32) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 7020624874560121172 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 1 (-40) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 0 ::
	Op3 (STX BPF_B BPF_MEM) 10 1 (-2) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 1680154682 ::
	Op3 (STX BPF_W BPF_MEM) 10 2 (-48) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 0 0 7306086830975370528 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 2 (-56) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 0 0 3198793151638750752 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 2 (-64) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 0 0 7308613580334851399 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 2 (-72) 0 ::
	Op3 (STX BPF_B BPF_MEM) 10 1 (-44) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 0 0 33896208940415604 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 2 (-80) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 0 0 7954894493527732000 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 2 (-88) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 0 0 7234298819098404210 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 2 (-96) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 0 0 2338339511083492417 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 2 (-104) 0 ::
	Op3 (STX BPF_B BPF_MEM) 10 1 (-110) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 1 0 0 30828 ::
	Op3 (STX BPF_HW BPF_MEM) 10 1 (-112) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 7792670397406667884 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 1 (-120) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 7792670345972511333 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 1 (-128) 0 ::
	Op0 (LD BPF_DW BPF_IMM) 1 0 0 8079567842339803203 ::
	Empty_line  ::
	Op3 (STX BPF_DW BPF_MEM) 10 1 (-136) 0 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 8 ::
	Op7 (ALU64 BPF_MOV) BPF_X 7 0 0 0 ::
	Op1 (LDX BPF_DW BPF_MEM) 1 6 168 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 10000 ::
	Op5 (JMP BPF_JGT) BPF_X 2 1 48 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 (-184) ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 16 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 16 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 6 0 0 ::
	Op0 (LD BPF_DW BPF_IMM) 2 1 0 0 ::
	Empty_line  ::
	Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 512 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 27 ::
	Op3 (STX BPF_W BPF_MEM) 10 0 (-168) 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 0 0 0 32 ::
	Op7 (ALU64 BPF_ARSH) BPF_K 0 0 0 32 ::
	Op5 (JMP BPF_JSGT) BPF_K 0 0 8 (-1) ::
	Op1 (LDX BPF_DW BPF_MEM) 5 6 128 0 ::
	Op1 (LDX BPF_DW BPF_MEM) 4 6 168 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 (-136) ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 27 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 7 0 0 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 6 ::
	Op5 (JMP BPF_GOTO) BPF_K 0 0 27 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 2 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 2 0 0 (-160) ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 6 0 0 ::
	Op7 (ALU64 BPF_MOV) BPF_K 3 0 0 24 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 56 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 0 0 0 ::
	Op7 (ALU64 BPF_LSH) BPF_K 1 0 0 32 ::
	Op7 (ALU64 BPF_RSH) BPF_K 1 0 0 32 ::
	Op5 (JMP BPF_JNE) BPF_K 1 0 7 0 ::
	Op1 (LDX BPF_DW BPF_MEM) 4 10 (-144) 0 ::
	Op1 (LDX BPF_DW BPF_MEM) 3 10 (-152) 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 (-40) ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 39 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 6 ::
	Op5 (JMP BPF_GOTO) BPF_K 0 0 5 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 (-72) ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 29 ::
	Op7 (ALU64 BPF_MOV) BPF_X 3 0 0 0 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 6 ::
	Op1 (LDX BPF_DW BPF_MEM) 3 6 176 0 ::
	Op5 (JMP BPF_JEQ) BPF_K 3 0 4 0 ::
	Op7 (ALU64 BPF_MOV) BPF_X 1 10 0 0 ::
	Op7 (ALU64 BPF_ADD) BPF_K 1 0 0 (-104) ::
	Op7 (ALU64 BPF_MOV) BPF_K 2 0 0 32 ::
	Op5 (BPF_CALL) BPF_K 0 0 0 6 ::
	Op7 (ALU64 BPF_MOV) BPF_K 0 0 0 0 ::
	Op5 (BPF_EXIT) BPF_K 0 0 0 0 :: nil.

Definition Real_tag := (Anormal nil Ebpf_Local_init (Other _ _ (Ebpf_map (Ez_val 0) (Ez_val 7) (Ez_val 4) (Ez_val 1016) (Ez_val 10000)) :: Ebpf_Sep_init) :: nil , O) :: nil.

Definition len := length STATEMENT.  

Definition After_exec := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) STATEMENT Real_tag len len .

Definition Need_to_check := Program_checker Real_tag After_exec.

Definition abc :=  Check_assertion Need_to_check.

Definition ab := No_error_all After_exec.

Definition Result := Program_exec Real_tag STATEMENT.