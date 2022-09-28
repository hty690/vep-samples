open BinInt
open BinNums
open BinPos
open Datatypes
open List_lemma

type unary_operation =
| Oneg
| Onot
| Shl32
| Right32

val eqb_unary : unary_operation -> unary_operation -> bool

type binary_operation =
| Oadd
| Osub
| Omul
| Opower
| Odiv
| Omod
| Oand
| Oor
| Oxor
| Oshl
| Oshr
| OAshl
| OAshr
| Oeq
| One
| Oend

val eqb_binary : binary_operation -> binary_operation -> bool

type list_operation =
| Vnthaddress
| Vnthval
| Vnth
| Vcons
| Vapp

val eqb_listop : list_operation -> list_operation -> bool

type expr_val =
| V_vari of ident
| Vfield_address of expr_val * ident
| Ez_val of coq_Z
| Vuop of unary_operation * expr_val
| Vbop of binary_operation * expr_val * expr_val
| Vlop of list_operation * expr_val * expr_val
| Vlist_vari of ident
| Vlist_sublist of expr_val * expr_val * expr_val
| Vlist_length of expr_val
| Vif of expr_val * expr_val * expr_val

val nullptr : expr_val

val coq_Vtrue : expr_val

val coq_Vfalse : expr_val

val get_val : expr_val -> expr_val

val eqb_val : expr_val -> expr_val -> bool

val coq_Find_list_val : expr_val -> bool

val coq_Used_in_val : expr_val -> expr_val -> bool

val coq_Changeval : expr_val -> expr_val -> expr_val -> expr_val

val coq_Changeval_option :
  expr_val -> expr_val -> expr_val option -> expr_val option

val coq_Simplify_uop : unary_operation -> expr_val -> expr_val

val coq_Simplify_bop : binary_operation -> expr_val -> expr_val -> expr_val

val coq_Simplify_lop : list_operation -> expr_val -> expr_val -> expr_val

val coq_Simplify_val_rec : expr_val -> expr_val

val coq_Simplify_val_ser : expr_val -> nat -> expr_val

val coq_Simplify_val : expr_val -> expr_val

val max_ident_in_expr : expr_val -> ident

type formal_part = expr_val list * coq_Z

type formal_expr_val = formal_part list

val formal_part_eqb : formal_part -> formal_part -> bool

val formal_expr_val_eqb : formal_part list -> formal_part list -> bool

val coq_Retransfer_part : expr_val list -> expr_val

val coq_Retransfer_formal : formal_expr_val -> expr_val

val coq_Retransfer : formal_expr_val -> expr_val

val coq_Neg_transfer_part : formal_part -> formal_part

val coq_Neg_transfer : formal_expr_val -> formal_expr_val

val coq_Add_transfer : formal_expr_val -> formal_expr_val -> formal_expr_val

val coq_Sub_transfer : formal_expr_val -> formal_expr_val -> formal_expr_val

val coq_Mul_one_one : formal_part -> formal_part -> formal_part

val coq_Mul_one_transfer : formal_part -> formal_expr_val -> formal_expr_val

val coq_Mul_transfer : formal_expr_val -> formal_expr_val -> formal_expr_val

val coq_Transfer : expr_val -> formal_expr_val

val coq_Transfer_expr_val : expr_val -> formal_expr_val
