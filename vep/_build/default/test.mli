open Assdef
open BinNums
open Datatypes
open EbpfExec
open Ebpfstate
open Ebpfstatement
open List
open Map_sep
open Sepdef
open Exprdef

val coq_STATEMENT : coq_Singleton_statement list

val coq_Real_tag : (ebpf_type assertion list * nat) list

val len : nat

val coq_After_exec :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list

val coq_Need_to_check :
  ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * ebpf_type
  assertion list) list

val abc :
  bool * ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
  list * ebpf_type assertion list) list

val ab : bool * nat list

val coq_Result :
  bool * ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
  list * ebpf_type assertion list) list
