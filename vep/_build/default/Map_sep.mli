open BinNums
open BinPos
open Datatypes
open List
open Sepdef
open Exprdef
open List_lemma

type ebpf_type =
| NOT_INIT
| SCALAR_VALUE
| PTR_TO_CTX
| CONST_PTR_TO_MAP
| PTR_TO_MAP_VALUE
| PTR_TO_MAP_VALUE_OR_NULL
| PTR_TO_STACK
| PTR_TO_PACKET
| PTR_TO_PACKET_END
| PTR_TO_SOCKET
| PTR_TO_SOCKET_OR_NULL

val eqb_ebpf : ebpf_type -> ebpf_type -> bool

type coq_Ebpf_t =
| Ebpf_map of expr_val * expr_val * expr_val * expr_val * expr_val
| Ebpf_context of expr_val * expr_val * expr_val * expr_val
| Accessable of expr_val * expr_val
| Map_element of expr_val * expr_val * expr_val * expr_val

val coq_Ebpf_sep : ebpf_type coq_Separation_def

type coq_Ebpf_Sep = (ebpf_type, coq_Ebpf_t) coq_Separation
