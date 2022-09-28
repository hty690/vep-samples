open BinInt
open BinNums

(** val coq_OXFO : coq_Z **)

let coq_OXFO =
  Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))

type coq_OpSource =
| BPF_X
| BPF_K

type coq_SizeModifier =
| BPF_W
| BPF_HW
| BPF_B
| BPF_DW

type coq_Atomic =
| BPF_ADDA
| BPF_ANDA
| BPF_ORA
| BPF_XORA
| BPF_ADDAF
| BPF_ANDAF
| BPF_ORAF
| BPF_XORAF
| BPF_XCHGA
| BPF_CMPXCHGA

(** val coq_Imm_to_Atomic : coq_Z -> coq_Atomic option **)

let coq_Imm_to_Atomic imm =
  let x = Z.coq_land imm coq_OXFO in
  (match x with
   | Z0 -> Some BPF_ADDA
   | Zpos p -> (match p with
                | Coq_xH -> Some BPF_ADDAF
                | _ -> None)
   | Zneg _ -> None)

type coq_ModeModifier =
| BPF_IMM
| BPF_ABS
| BPF_IND
| BPF_MEM
| BPF_LEN
| BPF_MSH
| BPF_ATOMIC

type coq_BPF_OP =
| BPF_ADD
| BPF_SUB
| BPF_MUL
| BPF_DIV
| BPF_OR
| BPF_AND
| BPF_LSH
| BPF_RSH
| BPF_NEG
| BPF_MOD
| BPF_XOR
| BPF_MOV
| BPF_ARSH
| BPF_END

type coq_BPF_JMP_OP =
| BPF_GOTO
| BPF_JEQ
| BPF_JGT
| BPF_JGE
| BPF_JSET
| BPF_JNE
| BPF_JSGT
| BPF_JSGE
| BPF_JLT
| BPF_JLE
| BPF_JSLT
| BPF_JSLE

type coq_BPF_LD =
| LD of coq_SizeModifier * coq_ModeModifier

type coq_BPF_LDX =
| LDX of coq_SizeModifier * coq_ModeModifier

type coq_BPF_ST =
| ST of coq_SizeModifier * coq_ModeModifier

type coq_BPF_STX =
| STX of coq_SizeModifier * coq_ModeModifier

type coq_BPF_ALU =
  coq_BPF_OP
  (* singleton inductive, whose constructor was ALU *)

type coq_BPF_JMP =
| JMP of coq_BPF_JMP_OP
| BPF_CALL
| BPF_EXIT

type coq_BPF_JMP32 =
  coq_BPF_JMP_OP
  (* singleton inductive, whose constructor was JMP32 *)

type coq_BPF_ALU64 =
  coq_BPF_OP
  (* singleton inductive, whose constructor was ALU64 *)

type coq_Singleton_statement =
| Empty_line
| Op0 of coq_BPF_LD * coq_Z * coq_Z * coq_Z * coq_Z
| Op1 of coq_BPF_LDX * coq_Z * coq_Z * coq_Z * coq_Z
| Op2 of coq_BPF_ST * coq_Z * coq_Z * coq_Z * coq_Z
| Op3 of coq_BPF_STX * coq_Z * coq_Z * coq_Z * coq_Z
| Op4 of coq_BPF_ALU * coq_OpSource * coq_Z * coq_Z * coq_Z * coq_Z
| Op5 of coq_BPF_JMP * coq_OpSource * coq_Z * coq_Z * coq_Z * coq_Z
| Op6 of coq_BPF_JMP32 * coq_OpSource * coq_Z * coq_Z * coq_Z * coq_Z
| Op7 of coq_BPF_ALU64 * coq_OpSource * coq_Z * coq_Z * coq_Z * coq_Z
