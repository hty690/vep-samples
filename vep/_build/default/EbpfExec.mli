open AssSolver
open Assdef
open BinInt
open BinNums
open Datatypes
open Ebpfop
open Ebpfstate
open Ebpfstatement
open Error
open List
open Map_sep
open Nat
open List_lemma

val look_up :
  nat -> coq_Singleton_statement list -> coq_Singleton_statement option

val coq_Remove_B_in_ProdAB :
  ('a2 -> 'a2 -> bool) -> ('a1 * 'a2) list -> 'a2 -> ('a1 * 'a2) list

val look_up_list_assertion :
  nat -> (ebpf_type assertion list * nat) list -> ebpf_type assertion list
  option

val coq_LD_exec :
  ebpf_type assertion -> nat -> coq_BPF_LD -> coq_Z -> coq_Z -> coq_Z ->
  coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat

val coq_LDX_exec :
  ebpf_type assertion -> nat -> coq_BPF_LDX -> coq_Z -> coq_Z -> coq_Z ->
  coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat

val coq_ST_exec :
  ebpf_type assertion -> nat -> coq_BPF_ST -> coq_Z -> coq_Z -> coq_Z ->
  coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat

val coq_STX_exec :
  ebpf_type assertion -> nat -> coq_BPF_STX -> coq_Z -> coq_Z -> coq_Z ->
  coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat

val coq_JMP_exec :
  ebpf_type assertion -> nat -> coq_BPF_JMP -> coq_OpSource -> coq_Z -> coq_Z
  -> coq_Z -> coq_Z -> ((ebpf_type assertion, ebpf_type coq_Prod_error)
  sum * nat) list

val coq_JMP32_exec :
  ebpf_type assertion -> nat -> coq_BPF_JMP32 -> coq_OpSource -> coq_Z ->
  coq_Z -> coq_Z -> coq_Z -> ((ebpf_type assertion, ebpf_type coq_Prod_error)
  sum * nat) list

val coq_ALU_exec :
  ebpf_type assertion -> nat -> coq_BPF_ALU -> coq_OpSource -> coq_Z -> coq_Z
  -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error)
  sum * nat

val coq_ALU64_exec :
  ebpf_type assertion -> nat -> coq_BPF_ALU64 -> coq_OpSource -> coq_Z ->
  coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error)
  sum * nat

val coq_Singleton_exec :
  (ebpf_type assertion * nat) -> coq_Singleton_statement list -> ((ebpf_type
  assertion, ebpf_type coq_Prod_error) sum * nat) list

val coq_Singleton_exec_list :
  (ebpf_type assertion, ebpf_type coq_Prod_error) sum list -> nat ->
  coq_Singleton_statement list -> ((ebpf_type assertion, ebpf_type
  coq_Prod_error) sum * nat) list

val update :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
  (ebpf_type assertion, ebpf_type coq_Prod_error) sum -> nat -> ((ebpf_type
  assertion, ebpf_type coq_Prod_error) sum list * nat) list

val list_update :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
  (ebpf_type assertion, ebpf_type coq_Prod_error) sum list -> nat ->
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list

val update_list :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list ->
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list

val list_update_list :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list

type coq_Divide_assertion = { coq_Front_part : ((ebpf_type assertion,
                                               ebpf_type coq_Prod_error)
                                               sum * nat) list;
                              coq_Back_part : ((ebpf_type assertion,
                                              ebpf_type coq_Prod_error)
                                              sum * nat) list;
                              coq_No_need : ((ebpf_type assertion, ebpf_type
                                            coq_Prod_error) sum * nat) list }

val coq_Divide :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list -> nat ->
  (ebpf_type assertion list * nat) list -> coq_Divide_assertion

val coq_Program_exec_once :
  nat -> nat -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
  list * nat) list -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
  list * nat) list -> coq_Singleton_statement list -> (ebpf_type assertion
  list * nat) list -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
  list * nat) list * ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
  list * nat) list

val coq_Program_exec_several :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
  coq_Singleton_statement list -> (ebpf_type assertion list * nat) list ->
  nat -> nat -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
  list * nat) list

val coq_Program_checker :
  (ebpf_type assertion list * nat) list -> ((ebpf_type assertion, ebpf_type
  coq_Prod_error) sum list * nat) list -> ((ebpf_type assertion, ebpf_type
  coq_Prod_error) sum list * ebpf_type assertion list) list

val coq_No_error :
  (ebpf_type assertion, ebpf_type coq_Prod_error) sum list -> bool

val coq_Check_assertion :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * ebpf_type
  assertion list) list -> bool * ((ebpf_type assertion, ebpf_type
  coq_Prod_error) sum list * ebpf_type assertion list) list

val coq_Program_exec :
  (ebpf_type assertion list * nat) list -> coq_Singleton_statement list ->
  bool * ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
  list * ebpf_type assertion list) list

val coq_No_error_all :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
  bool * nat list
