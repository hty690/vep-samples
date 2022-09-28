open Assdef
open BinInt
open BinNums
open BinPos
open Datatypes
open Ebpf_helper
open Ebpfstate
open Ebpfstatement
open Error
open Funcspec
open List
open Map_sep
open Propdef
open Propsolver
open Sepdef
open Exprdef
open List_lemma

val coq_Bpf_eval_l : coq_Z -> expr_val

val coq_Bpf_eval_val_rec : expr_val -> coq_Ebpf_Sep list -> expr_val option

val coq_Bpf_eval_val : expr_val -> coq_Ebpf_Sep list -> expr_val option

val coq_Bpf_eval_r : coq_Z -> coq_Ebpf_Sep list -> expr_val option

val coq_Valid_load :
  expr_val -> coq_Proposition list -> coq_Ebpf_Sep list -> expr_val option

val coq_Change_prop :
  coq_Ebpf_Sep list -> coq_Proposition list -> expr_val -> expr_val ->
  (coq_Ebpf_Sep list * coq_Proposition list) option

val coq_Bpf_store :
  expr_val -> expr_val -> ebpf_type -> coq_Ebpf_Sep list -> coq_Proposition
  list -> ((ebpf_type, ebpf_type coq_T) coq_Separation list * coq_Proposition
  list) option

val coq_Bpf_op :
  coq_BPF_OP -> expr_val option -> expr_val option -> expr_val option

val coq_Bpf_op_K :
  ebpf_type assertion -> coq_BPF_OP -> coq_Z -> coq_Z -> (ebpf_type
  assertion, ebpf_type coq_Prod_error) sum

val coq_Bpf_op_X :
  ebpf_type assertion -> coq_BPF_OP -> coq_Z -> coq_Z -> (ebpf_type
  assertion, ebpf_type coq_Prod_error) sum

val coq_Bpf_mem :
  coq_Z -> coq_Z -> coq_Z -> coq_Ebpf_Sep list -> coq_Proposition list ->
  ((ebpf_type, ebpf_type coq_T) coq_Separation list * coq_Proposition list)
  option

val coq_Bpf_mem_imm :
  coq_Z -> coq_Z -> coq_Z -> coq_Ebpf_Sep list -> coq_Proposition list ->
  ((ebpf_type, ebpf_type coq_T) coq_Separation list * coq_Proposition list)
  option

val coq_Bpf_atomic :
  coq_Z -> coq_Z -> coq_Z -> coq_Atomic option -> coq_Ebpf_Sep list ->
  coq_Proposition list -> ((ebpf_type, ebpf_type coq_T) coq_Separation
  list * coq_Proposition list) option

val coq_Bpf_LD :
  ebpf_type assertion -> coq_SizeModifier -> coq_ModeModifier -> coq_Z ->
  coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error)
  sum

val coq_Bpf_LDX :
  ebpf_type assertion -> coq_SizeModifier -> coq_ModeModifier -> coq_Z ->
  coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error)
  sum

val coq_Bpf_ST :
  ebpf_type assertion -> coq_SizeModifier -> coq_ModeModifier -> coq_Z ->
  coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error)
  sum

val coq_Bpf_STX :
  ebpf_type assertion -> coq_SizeModifier -> coq_ModeModifier -> coq_Z ->
  coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error)
  sum

val coq_Add_new_Prop :
  coq_BPF_JMP_OP -> expr_val option -> expr_val option -> coq_Proposition
  list -> coq_Proposition option * coq_Proposition list option

val coq_Bpf_jmp_X :
  ebpf_type assertion -> nat -> coq_BPF_JMP_OP -> coq_Z -> coq_Z -> coq_Z ->
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list

val coq_Bpf_jmp_K :
  ebpf_type assertion -> nat -> coq_BPF_JMP_OP -> coq_Z -> coq_Z -> coq_Z ->
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list

val coq_Bpf_jmp :
  ebpf_type assertion -> nat -> coq_BPF_JMP_OP -> coq_OpSource -> coq_Z ->
  coq_Z -> coq_Z -> coq_Z -> ((ebpf_type assertion, ebpf_type coq_Prod_error)
  sum * nat) list

val coq_Assertion_ignore_return :
  ebpf_type coq_Prod_ret list -> nat -> ((ebpf_type assertion, ebpf_type
  coq_Prod_error) sum * nat) list

val coq_Bpf_call_num :
  ebpf_type assertion -> ebpf_type funcdes list -> expr_val option list ->
  nat -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list

val coq_Find_id_in_Helper :
  (nat * ebpf_type funcspec) list -> ident -> (nat * ebpf_type funcdes list)
  option

val eval_arg : ebpf_type assertion -> nat -> expr_val option list

val coq_Bpf_call :
  ebpf_type assertion -> nat -> ident -> ((ebpf_type assertion, ebpf_type
  coq_Prod_error) sum * nat) list
