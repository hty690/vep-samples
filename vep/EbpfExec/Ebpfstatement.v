(* This file contain the definition of statments of Ebpf programs*)
From VSTCForm Require Export VSTCFormlib.

Definition OXFO := 240%Z.

Inductive OpSource : Type :=
  | BPF_X : OpSource  (* use 'src_reg' register as source operand *)
  | BPF_K : OpSource  (* use 32-bit immediate as source operand *)
.

Inductive SizeModifier : Type :=
  | BPF_W : SizeModifier  (* 0x00 word = 4 byte*)
  | BPF_HW : SizeModifier  (* 0x08 half word = 2 byte *)
  | BPF_B : SizeModifier  (* 0x10 byte *)
  | BPF_DW : SizeModifier (* 0x18 double word = 8 byte eBPF only*)
.

Inductive Atomic : Type :=
  | BPF_ADDA : Atomic   (* 0x00 *)
  | BPF_ANDA : Atomic   (* 0x50 *)
  | BPF_ORA : Atomic    (* 0x40 *)
  | BPF_XORA : Atomic   (* 0xa0 *)
  | BPF_ADDAF : Atomic   (* 0x01 *)   (* BPF_ADD | BPF_FETCH *)
  | BPF_ANDAF : Atomic   (* 0x51 *)   (* BPF_AND | BPF_FETCH *)
  | BPF_ORAF : Atomic    (* 0x41 *)   (* BPF_OR  | BPF_FETCH *)
  | BPF_XORAF : Atomic   (* 0xa1 *)   (* BPF_XOR | BPF_FETCH *)
  | BPF_XCHGA : Atomic  (* 0xe1 *)
  | BPF_CMPXCHGA : Atomic  (* 0xf1 *)
.

Definition Imm_to_Atomic (imm : Z) : option Atomic :=
  let x := Z.land imm OXFO in
    match x with
      | 0 => Some BPF_ADDA
      | 1 => Some BPF_ADDAF
      | _ => None
    end.

Inductive ModeModifier : Type :=
  | BPF_IMM : ModeModifier  (* 0x00 used for 32-bit mov in classic BPF and 64-bit in eBPF *)
  | BPF_ABS : ModeModifier  (* 0x20 *)
  | BPF_IND : ModeModifier  (* 0x40 *)
  | BPF_MEM : ModeModifier  (* 0x60 *)
  | BPF_LEN : ModeModifier  (* 0x80 classic BPF only, reserved in eBPF *)
  | BPF_MSH : ModeModifier  (* 0xa0 classic BPF only, reserved in eBPF *)
  | BPF_ATOMIC : ModeModifier  (* 0xc0 eBPF only, atomic operations, use imm for extra encoding *)
.

Inductive BPF_OP : Type :=
  | BPF_ADD : BPF_OP  (* 0x00 *)
  | BPF_SUB : BPF_OP  (* 0x10 *)
  | BPF_MUL : BPF_OP  (* 0x20 *)
  | BPF_DIV : BPF_OP  (* 0x30 *)
  | BPF_OR  : BPF_OP  (* 0x40 *)
  | BPF_AND : BPF_OP  (* 0x50 *) 
  | BPF_LSH : BPF_OP  (* 0x60 *)
  | BPF_RSH : BPF_OP  (* 0x70 *)
  | BPF_NEG : BPF_OP  (* 0x80 *)
  | BPF_MOD : BPF_OP  (* 0x90 *)
  | BPF_XOR : BPF_OP  (* 0xa0 *)
  | BPF_MOV : BPF_OP  (* 0xb0  eBPF only: mov reg to reg *)
  | BPF_ARSH : BPF_OP (* 0xc0  eBPF only: sign extending shift right *)
  | BPF_END : BPF_OP  (* 0xd0  eBPF only: endianness conversion *)
.

Inductive BPF_JMP_OP : Type :=
  | BPF_GOTO : BPF_JMP_OP (* 0x00 *)
  | BPF_JEQ : BPF_JMP_OP (* 0x10 *)
  | BPF_JGT : BPF_JMP_OP (* 0x20 *)
  | BPF_JGE : BPF_JMP_OP (* 0x30 *)
  | BPF_JSET : BPF_JMP_OP (* 0x40 *)
  | BPF_JNE : BPF_JMP_OP (* 0x50  eBPF only: jump != *)
  | BPF_JSGT : BPF_JMP_OP (* 0x60  eBPF only: signed '>' *)
  | BPF_JSGE : BPF_JMP_OP (* 0x70  eBPF only: signed '>=' *)
  | BPF_JLT : BPF_JMP_OP  (* 0xa0  eBPF only: unsigned '<' *)
  | BPF_JLE : BPF_JMP_OP  (* 0xb0  eBPF only: unsigned '<=' *)
  | BPF_JSLT : BPF_JMP_OP (* 0xc0  eBPF only: signed '<' *)
  | BPF_JSLE : BPF_JMP_OP (* 0xd0  eBPF only: signed '<=' *)
.

(* 3bits mode + 2bits size + 3bits instruction class *)

Inductive BPF_LD : Type := | LD : SizeModifier -> ModeModifier -> BPF_LD. 

Inductive BPF_LDX : Type := | LDX : SizeModifier -> ModeModifier -> BPF_LDX.

Inductive BPF_ST : Type := | ST : SizeModifier -> ModeModifier -> BPF_ST.

Inductive BPF_STX : Type := | STX : SizeModifier -> ModeModifier -> BPF_STX.


(* 4bits operation code + 1bit source + 3bits instruction class *)
Inductive BPF_ALU : Type :=
  | ALU : BPF_OP -> BPF_ALU
.

Inductive BPF_JMP : Type :=
  | JMP : BPF_JMP_OP -> BPF_JMP
  | BPF_CALL : BPF_JMP (* 0x80  eBPF BPF_JMP only: function call *)
  | BPF_EXIT : BPF_JMP (* 0x90  eBPF BPF_JMP only: function return *)
.

Inductive BPF_JMP32 : Type :=
  | JMP32 : BPF_JMP_OP -> BPF_JMP32
.

Inductive BPF_ALU64 : Type :=
  | ALU64 : BPF_OP -> BPF_ALU64
.

Inductive Singleton_statement: Type := 
  | Empty_line : Singleton_statement
  | Op0 : BPF_LD -> Z -> Z -> Z -> Z -> Singleton_statement     (*0x00*)
  | Op1 : BPF_LDX -> Z -> Z -> Z -> Z -> Singleton_statement    (*0x01*)
  | Op2 : BPF_ST -> Z -> Z -> Z -> Z -> Singleton_statement     (*0x02*)
  | Op3 : BPF_STX -> Z -> Z -> Z -> Z -> Singleton_statement    (*0x03*)
  | Op4 : BPF_ALU -> OpSource -> Z -> Z -> Z -> Z -> Singleton_statement    (*0x04*)
  | Op5 : BPF_JMP -> OpSource -> Z -> Z -> Z -> Z -> Singleton_statement    (*0x05*)
  | Op6 : BPF_JMP32 -> OpSource -> Z -> Z -> Z -> Z -> Singleton_statement  (*0x06*)
  | Op7 : BPF_ALU64 -> OpSource -> Z -> Z -> Z -> Z -> Singleton_statement  (*0x07*)
.

(* 8bits op + 4bits dest + 4bits src + 16bits signed off + 32bits signed immediate constant*)


Inductive Ebpf_error : Type :=
  | All_clear : Ebpf_error
  | No_precondition : Ebpf_error
  | Entailment_check_fail : Ebpf_error
  | Execuation_fail : Ebpf_error
  | Several_error : Ebpf_error -> Ebpf_error -> Ebpf_error
.

(* error message *)