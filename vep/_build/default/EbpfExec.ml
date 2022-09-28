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

(** val look_up :
    nat -> coq_Singleton_statement list -> coq_Singleton_statement option **)

let rec look_up n = function
| [] -> None
| a :: s' -> (match n with
              | O -> Some a
              | S n' -> look_up n' s')

(** val coq_Remove_B_in_ProdAB :
    ('a2 -> 'a2 -> bool) -> ('a1 * 'a2) list -> 'a2 -> ('a1 * 'a2) list **)

let rec coq_Remove_B_in_ProdAB eqbB l b =
  match l with
  | [] -> []
  | p :: l' ->
    let (a, b') = p in
    if eqbB b b'
    then coq_Remove_B_in_ProdAB eqbB l' b
    else (a, b') :: (coq_Remove_B_in_ProdAB eqbB l' b)

(** val look_up_list_assertion :
    nat -> (ebpf_type assertion list * nat) list -> ebpf_type assertion list
    option **)

let look_up_list_assertion n l =
  coq_Find_B_in_prodAB PeanoNat.Nat.eqb l n

(** val coq_LD_exec :
    ebpf_type assertion -> nat -> coq_BPF_LD -> coq_Z -> coq_Z -> coq_Z ->
    coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat **)

let coq_LD_exec a pc bLD dest src off imm =
  let LD (sizeM, modeM) = bLD in
  ((coq_Bpf_LD a sizeM modeM dest src off imm), (S pc))

(** val coq_LDX_exec :
    ebpf_type assertion -> nat -> coq_BPF_LDX -> coq_Z -> coq_Z -> coq_Z ->
    coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat **)

let coq_LDX_exec a pc bLDX dest src off imm =
  let LDX (sizeM, modeM) = bLDX in
  ((coq_Bpf_LDX a sizeM modeM dest src off imm), (S pc))

(** val coq_ST_exec :
    ebpf_type assertion -> nat -> coq_BPF_ST -> coq_Z -> coq_Z -> coq_Z ->
    coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat **)

let coq_ST_exec a pc bST dest src off imm =
  let ST (sizeM, modeM) = bST in
  ((coq_Bpf_ST a sizeM modeM dest src off imm), (S pc))

(** val coq_STX_exec :
    ebpf_type assertion -> nat -> coq_BPF_STX -> coq_Z -> coq_Z -> coq_Z ->
    coq_Z -> (ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat **)

let coq_STX_exec a pc bSTX dest src off imm =
  let STX (sizeM, modeM) = bSTX in
  ((coq_Bpf_STX a sizeM modeM dest src off imm), (S pc))

(** val coq_JMP_exec :
    ebpf_type assertion -> nat -> coq_BPF_JMP -> coq_OpSource -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum * nat) list **)

let coq_JMP_exec a pc bJMP os dest src off imm =
  match bJMP with
  | JMP op -> coq_Bpf_jmp a pc op os dest src off imm
  | BPF_CALL -> coq_Bpf_call a pc (Z.to_pos imm)
  | BPF_EXIT ->
    let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list =
      sepx; coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
    in
    (match coq_Bpf_eval_r Z0 (Obj.magic sepx) with
     | Some _ ->
       ((Coq_inl
         (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = propx;
           coq_Local_list = localx; coq_Sep_list = sepx; coq_Exist_list =
           exist_list })), (S pc))
     | None ->
       ((Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
         localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
         coq_Errorm = (Coq_error Invalid_operation) }), inf)) :: []

(** val coq_JMP32_exec :
    ebpf_type assertion -> nat -> coq_BPF_JMP32 -> coq_OpSource -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum * nat) list **)

let coq_JMP32_exec =
  coq_Bpf_jmp

(** val coq_ALU_exec :
    ebpf_type assertion -> nat -> coq_BPF_ALU -> coq_OpSource -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type
    coq_Prod_error) sum * nat **)

let coq_ALU_exec a pc bALU os dest src _ imm =
  match os with
  | BPF_X -> ((coq_Bpf_op_X a bALU dest src), (S pc))
  | BPF_K -> ((coq_Bpf_op_K a bALU dest imm), (S pc))

(** val coq_ALU64_exec :
    ebpf_type assertion -> nat -> coq_BPF_ALU64 -> coq_OpSource -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type
    coq_Prod_error) sum * nat **)

let coq_ALU64_exec a pc bALU64 os dest src _ imm =
  match os with
  | BPF_X -> ((coq_Bpf_op_X a bALU64 dest src), (S pc))
  | BPF_K -> ((coq_Bpf_op_K a bALU64 dest imm), (S pc))

(** val coq_Singleton_exec :
    (ebpf_type assertion * nat) -> coq_Singleton_statement list ->
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list **)

let coq_Singleton_exec a sl =
  let s = look_up (snd a) sl in
  (match s with
   | Some s0 ->
     (match s0 with
      | Empty_line -> ((Coq_inl (fst a)), (S (snd a))) :: []
      | Op0 (bLD, dest, src, off, imm) ->
        (coq_LD_exec (fst a) (snd a) bLD dest src off imm) :: []
      | Op1 (bLDX, dest, src, off, imm) ->
        (coq_LDX_exec (fst a) (snd a) bLDX dest src off imm) :: []
      | Op2 (bST, dest, src, off, imm) ->
        (coq_ST_exec (fst a) (snd a) bST dest src off imm) :: []
      | Op3 (bSTX, dest, src, off, imm) ->
        (coq_STX_exec (fst a) (snd a) bSTX dest src off imm) :: []
      | Op4 (bALU, os, dest, src, off, imm) ->
        (coq_ALU_exec (fst a) (snd a) bALU os dest src off imm) :: []
      | Op5 (bJMP, os, dest, src, off, imm) ->
        coq_JMP_exec (fst a) (snd a) bJMP os dest src off imm
      | Op6 (bJMP32, os, dest, src, off, imm) ->
        coq_JMP32_exec (fst a) (snd a) bJMP32 os dest src off imm
      | Op7 (bALU64, os, dest, src, off, imm) ->
        (coq_ALU64_exec (fst a) (snd a) bALU64 os dest src off imm) :: [])
   | None -> [])

(** val coq_Singleton_exec_list :
    (ebpf_type assertion, ebpf_type coq_Prod_error) sum list -> nat ->
    coq_Singleton_statement list -> ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum * nat) list **)

let rec coq_Singleton_exec_list a pc s =
  match a with
  | [] -> []
  | a' :: l ->
    app
      (match a' with
       | Coq_inl a0 -> coq_Singleton_exec (a0, pc) s
       | Coq_inr _ -> (a', inf) :: []) (coq_Singleton_exec_list l pc s)

(** val update :
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
    (ebpf_type assertion, ebpf_type coq_Prod_error) sum -> nat -> ((ebpf_type
    assertion, ebpf_type coq_Prod_error) sum list * nat) list **)

let update l a pc =
  match coq_Find_B_in_prodAB PeanoNat.Nat.eqb l pc with
  | Some a' ->
    ((a :: a'), pc) :: (coq_Remove_B_in_ProdAB PeanoNat.Nat.eqb l pc)
  | None -> ((a :: []), pc) :: l

(** val list_update :
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
    (ebpf_type assertion, ebpf_type coq_Prod_error) sum list -> nat ->
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list **)

let list_update l a pc =
  match coq_Find_B_in_prodAB PeanoNat.Nat.eqb l pc with
  | Some a' ->
    ((app a a'), pc) :: (coq_Remove_B_in_ProdAB PeanoNat.Nat.eqb l pc)
  | None -> (a, pc) :: l

(** val update_list :
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list ->
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list **)

let rec update_list l = function
| [] -> l
| p :: l' -> let (a, pc) = p in update (update_list l l') a pc

(** val list_update_list :
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list **)

let rec list_update_list l = function
| [] -> l
| p :: l' -> let (a, pc) = p in list_update (list_update_list l l') a pc

type coq_Divide_assertion = { coq_Front_part : ((ebpf_type assertion,
                                               ebpf_type coq_Prod_error)
                                               sum * nat) list;
                              coq_Back_part : ((ebpf_type assertion,
                                              ebpf_type coq_Prod_error)
                                              sum * nat) list;
                              coq_No_need : ((ebpf_type assertion, ebpf_type
                                            coq_Prod_error) sum * nat) list }

(** val coq_Divide :
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list -> nat
    -> (ebpf_type assertion list * nat) list -> coq_Divide_assertion **)

let rec coq_Divide l pc real_tag =
  match l with
  | [] -> { coq_Front_part = []; coq_Back_part = []; coq_No_need = [] }
  | p :: l' ->
    let (a, pc') = p in
    let res = coq_Divide l' pc real_tag in
    (match coq_Find_B_in_prodAB PeanoNat.Nat.eqb real_tag pc' with
     | Some _ ->
       { coq_Front_part = res.coq_Front_part; coq_Back_part =
         res.coq_Back_part; coq_No_need = ((a, pc') :: res.coq_No_need) }
     | None ->
       if PeanoNat.Nat.ltb pc' pc
       then { coq_Front_part = ((a, pc') :: res.coq_Front_part);
              coq_Back_part = res.coq_Back_part; coq_No_need =
              res.coq_No_need }
       else { coq_Front_part = res.coq_Front_part; coq_Back_part = ((a,
              pc') :: res.coq_Back_part); coq_No_need = res.coq_No_need })

(** val coq_Program_exec_once :
    nat -> nat -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
    list * nat) list -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
    list * nat) list -> coq_Singleton_statement list -> (ebpf_type assertion
    list * nat) list -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
    list * nat) list * ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
    list * nat) list **)

let rec coq_Program_exec_once time pc l front_assertion s real_tag =
  match time with
  | O -> ([], [])
  | S n' ->
    let need_to_exec =
      match coq_Find_B_in_prodAB PeanoNat.Nat.eqb l pc with
      | Some a ->
        (match coq_Find_B_in_prodAB PeanoNat.Nat.eqb front_assertion pc with
         | Some b -> app a b
         | None -> a)
      | None ->
        (match coq_Find_B_in_prodAB PeanoNat.Nat.eqb front_assertion pc with
         | Some a -> a
         | None -> [])
    in
    let results = coq_Singleton_exec_list need_to_exec pc s in
    let res = coq_Divide results pc real_tag in
    let (virtual_tags, next_round) =
      coq_Program_exec_once n' (add pc (S O)) l
        (update_list front_assertion res.coq_Front_part) s real_tag
    in
    ((update_list virtual_tags res.coq_No_need),
    (update_list next_round res.coq_Back_part))

(** val coq_Program_exec_several :
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
    coq_Singleton_statement list -> (ebpf_type assertion list * nat) list ->
    nat -> nat -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
    list * nat) list **)

let rec coq_Program_exec_several l s real_list time len =
  match time with
  | O -> l
  | S n' ->
    let (virtual_tag, next_round) =
      coq_Program_exec_once len O l [] s real_list
    in
    let virtual_tags = list_update_list virtual_tag l in
    (match next_round with
     | [] -> virtual_tags
     | _ :: _ ->
       list_update_list virtual_tags
         (coq_Program_exec_several next_round s real_list n' len))

(** val coq_Program_checker :
    (ebpf_type assertion list * nat) list -> ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum list * nat) list -> ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum list * ebpf_type assertion list) list **)

let rec coq_Program_checker real_tag = function
| [] -> []
| p :: l' ->
  let (a, pc) = p in
  (match look_up_list_assertion pc real_tag with
   | Some v -> (a, v) :: (coq_Program_checker real_tag l')
   | None -> coq_Program_checker real_tag l')

(** val coq_No_error :
    (ebpf_type assertion, ebpf_type coq_Prod_error) sum list -> bool **)

let rec coq_No_error = function
| [] -> true
| a :: l' -> (match a with
              | Coq_inl _ -> coq_No_error l'
              | Coq_inr _ -> false)

(** val coq_Check_assertion :
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * ebpf_type
    assertion list) list -> bool * ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum list * ebpf_type assertion list) list **)

let rec coq_Check_assertion = function
| [] -> (true, [])
| p :: l' ->
  let (s, v) = p in
  let (flag, ls) = coq_Check_assertion l' in
  if (&&) (coq_No_error s)
       (coq_Ass_list_solver coq_Ebpf_sep v
         (coq_Clear_Prod_error coq_Ebpf_sep s))
  then (flag, ls)
  else (false, ((s, v) :: ls))

(** val coq_Program_exec :
    (ebpf_type assertion list * nat) list -> coq_Singleton_statement list ->
    bool * ((ebpf_type assertion, ebpf_type coq_Prod_error) sum
    list * ebpf_type assertion list) list **)

let coq_Program_exec real_tag s =
  let n = length s in
  let virtual_tag =
    coq_Program_exec_several
      (map (fun a -> ((map (fun b -> Coq_inl b) (fst a)), (snd a))) real_tag)
      s real_tag n n
  in
  coq_Check_assertion (coq_Program_checker real_tag virtual_tag)

(** val coq_No_error_all :
    ((ebpf_type assertion, ebpf_type coq_Prod_error) sum list * nat) list ->
    bool * nat list **)

let rec coq_No_error_all = function
| [] -> (true, [])
| p :: l' ->
  let (a, pc) = p in
  if coq_No_error a
  then coq_No_error_all l'
  else (false, (pc :: (snd (coq_No_error_all l'))))
