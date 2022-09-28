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

(** val coq_Bpf_eval_l : coq_Z -> expr_val **)

let coq_Bpf_eval_l reg =
  V_vari (Z.to_pos (Z.add reg (Zpos Coq_xH)))

(** val coq_Bpf_eval_val_rec :
    expr_val -> coq_Ebpf_Sep list -> expr_val option **)

let rec coq_Bpf_eval_val_rec x = function
| [] -> None
| h :: s' ->
  (match h with
   | Data_at (x1, ty, x2) ->
     if (&&) (eqb_val x2 x) (negb (coq_Ebpf_sep.eqb_ptype ty NOT_INIT))
     then (match coq_Bpf_eval_val_rec x s' with
           | Some x' -> Some x'
           | None -> Some x1)
     else coq_Bpf_eval_val_rec x s'
   | Memory (x1, _, s0) ->
     if eqb_val x1 x then Some s0 else coq_Bpf_eval_val_rec x s'
   | Other e ->
     (match e with
      | Ebpf_context (x1, st, ed, s0) ->
        if eqb_val x (coq_Simplify_val (Vbop (Oadd, x1, st)))
        then Some coq_Context_list
        else if eqb_val x (coq_Simplify_val (Vbop (Oadd, x1, ed)))
             then Some data_ed
             else if eqb_val x x1 then Some s0 else coq_Bpf_eval_val_rec x s'
      | _ -> coq_Bpf_eval_val_rec x s')
   | _ -> coq_Bpf_eval_val_rec x s')

(** val coq_Bpf_eval_val :
    expr_val -> coq_Ebpf_Sep list -> expr_val option **)

let coq_Bpf_eval_val x l =
  coq_Bpf_eval_val_rec (coq_Simplify_val x) l

(** val coq_Bpf_eval_r : coq_Z -> coq_Ebpf_Sep list -> expr_val option **)

let coq_Bpf_eval_r reg sepx =
  coq_Bpf_eval_val (coq_Bpf_eval_l reg) sepx

(** val coq_Valid_load :
    expr_val -> coq_Proposition list -> coq_Ebpf_Sep list -> expr_val option **)

let coq_Valid_load x _ _ =
  Some (coq_Simplify_val x)

(** val coq_Change_prop :
    coq_Ebpf_Sep list -> coq_Proposition list -> expr_val -> expr_val ->
    (coq_Ebpf_Sep list * coq_Proposition list) option **)

let coq_Change_prop sepx propx reg x =
  if coq_Find_list_val reg
  then Some (sepx, (coq_Change_prop_vequal propx reg x))
  else if coq_Used_in_val reg (V_vari (Coq_xI (Coq_xO (Coq_xI Coq_xH))))
       then Some (sepx, (coq_Change_prop_vequal propx reg x))
       else None

(** val coq_Bpf_store :
    expr_val -> expr_val -> ebpf_type -> coq_Ebpf_Sep list -> coq_Proposition
    list -> ((ebpf_type, ebpf_type coq_T) coq_Separation
    list * coq_Proposition list) option **)

let coq_Bpf_store reg x ty sepx propx =
  match coq_Find_in_Sep_list_address coq_Ebpf_sep reg (Obj.magic sepx) with
  | Some w ->
    (match w with
     | Other t ->
       (match Obj.magic t with
        | Ebpf_context (_, _, _, _) -> Some ((Obj.magic sepx), propx)
        | _ ->
          let (ns, flag) =
            coq_Change_sepx_typed coq_Ebpf_sep (Obj.magic sepx) reg x ty
          in
          if flag then Some (ns, propx) else None)
     | _ ->
       let (ns, flag) =
         coq_Change_sepx_typed coq_Ebpf_sep (Obj.magic sepx) reg x ty
       in
       if flag then Some (ns, propx) else None)
  | None -> Obj.magic coq_Change_prop sepx propx reg x

(** val coq_Bpf_op :
    coq_BPF_OP -> expr_val option -> expr_val option -> expr_val option **)

let coq_Bpf_op op x1 x2 =
  match x1 with
  | Some v1 ->
    (match x2 with
     | Some v2 ->
       (match op with
        | BPF_ADD -> Some (coq_Simplify_val (Vbop (Oadd, v1, v2)))
        | BPF_SUB -> Some (coq_Simplify_val (Vbop (Osub, v1, v2)))
        | BPF_MUL -> Some (coq_Simplify_val (Vbop (Omul, v1, v2)))
        | BPF_DIV -> Some (coq_Simplify_val (Vbop (Odiv, v1, v2)))
        | BPF_OR -> Some (coq_Simplify_val (Vbop (Oor, v1, v2)))
        | BPF_AND -> Some (coq_Simplify_val (Vbop (Oand, v1, v2)))
        | BPF_LSH -> Some (coq_Simplify_val (Vbop (Oshl, v1, v2)))
        | BPF_RSH -> Some (coq_Simplify_val (Vbop (Oshr, v1, v2)))
        | BPF_NEG -> Some (coq_Simplify_val (Vuop (Oneg, v1)))
        | BPF_MOD -> Some (coq_Simplify_val (Vbop (Omod, v1, v2)))
        | BPF_XOR -> Some (coq_Simplify_val (Vbop (Oxor, v1, v2)))
        | BPF_MOV -> x2
        | BPF_ARSH -> Some (coq_Simplify_val (Vbop (OAshl, v1, v2)))
        | BPF_END -> Some (coq_Simplify_val (Vbop (Oend, v1, v2))))
     | None -> None)
  | None ->
    (match x2 with
     | Some _ -> (match op with
                  | BPF_MOV -> x2
                  | _ -> None)
     | None -> None)

(** val coq_Bpf_op_K :
    ebpf_type assertion -> coq_BPF_OP -> coq_Z -> coq_Z -> (ebpf_type
    assertion, ebpf_type coq_Prod_error) sum **)

let coq_Bpf_op_K a op dest imm =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
  in
  (match coq_Bpf_op op (coq_Bpf_eval_r dest (Obj.magic sepx)) (Some (Ez_val
           imm)) with
   | Some w ->
     (match coq_Bpf_store (coq_Bpf_eval_l dest) w SCALAR_VALUE
              (Obj.magic sepx) propx with
      | Some p ->
        let (s', w') = p in
        Coq_inl
        (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = w'; coq_Local_list =
          localx; coq_Sep_list = s'; coq_Exist_list = exist_list })
      | None ->
        Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_operation) })
   | None ->
     Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
       localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
       coq_Errorm = (Coq_error Invalid_operation) })

(** val coq_Bpf_op_X :
    ebpf_type assertion -> coq_BPF_OP -> coq_Z -> coq_Z -> (ebpf_type
    assertion, ebpf_type coq_Prod_error) sum **)

let coq_Bpf_op_X a op dest src =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
  in
  (match coq_Bpf_op op (coq_Bpf_eval_r dest (Obj.magic sepx))
           (coq_Bpf_eval_r src (Obj.magic sepx)) with
   | Some w ->
     (match coq_Bpf_store (coq_Bpf_eval_l dest) w SCALAR_VALUE
              (Obj.magic sepx) propx with
      | Some p ->
        let (s', w') = p in
        Coq_inl
        (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = w'; coq_Local_list =
          localx; coq_Sep_list = s'; coq_Exist_list = exist_list })
      | None ->
        Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_operation) })
   | None ->
     Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
       localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
       coq_Errorm = (Coq_error Invalid_operation) })

(** val coq_Bpf_mem :
    coq_Z -> coq_Z -> coq_Z -> coq_Ebpf_Sep list -> coq_Proposition list ->
    ((ebpf_type, ebpf_type coq_T) coq_Separation list * coq_Proposition list)
    option **)

let coq_Bpf_mem dest src off sepx propx =
  match coq_Bpf_eval_r dest sepx with
  | Some v1 ->
    (match coq_Bpf_eval_r src sepx with
     | Some v2 ->
       let v = coq_Valid_load (Vbop (Oadd, v1, (Ez_val off))) propx sepx in
       (match v with
        | Some v' ->
          coq_Bpf_store (coq_Simplify_val (Vbop (Oadd, v', (Ez_val off)))) v2
            SCALAR_VALUE sepx propx
        | None -> None)
     | None -> None)
  | None -> None

(** val coq_Bpf_mem_imm :
    coq_Z -> coq_Z -> coq_Z -> coq_Ebpf_Sep list -> coq_Proposition list ->
    ((ebpf_type, ebpf_type coq_T) coq_Separation list * coq_Proposition list)
    option **)

let coq_Bpf_mem_imm dest off imm sepx propx =
  match coq_Bpf_eval_r dest sepx with
  | Some v1 ->
    let v = coq_Valid_load (Vbop (Oadd, v1, (Ez_val off))) propx sepx in
    (match v with
     | Some v' ->
       coq_Bpf_store (coq_Simplify_val (Vbop (Oadd, v', (Ez_val off))))
         (Ez_val imm) SCALAR_VALUE sepx propx
     | None -> None)
  | None -> None

(** val coq_Bpf_atomic :
    coq_Z -> coq_Z -> coq_Z -> coq_Atomic option -> coq_Ebpf_Sep list ->
    coq_Proposition list -> ((ebpf_type, ebpf_type coq_T) coq_Separation
    list * coq_Proposition list) option **)

let coq_Bpf_atomic dest src off op sepx propx =
  match op with
  | Some op' ->
    (match op' with
     | BPF_ADDA ->
       (match coq_Bpf_op BPF_ADD
                (coq_Bpf_eval_val
                  (coq_Simplify_val (Vbop (Oadd, (coq_Bpf_eval_l dest),
                    (Ez_val off)))) sepx) (coq_Bpf_eval_r src sepx) with
        | Some w ->
          coq_Bpf_store
            (coq_Simplify_val (Vbop (Oadd, (coq_Bpf_eval_l dest), (Ez_val
              off)))) w SCALAR_VALUE sepx propx
        | None -> None)
     | _ -> None)
  | None -> None

(** val coq_Bpf_LD :
    ebpf_type assertion -> coq_SizeModifier -> coq_ModeModifier -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type
    coq_Prod_error) sum **)

let coq_Bpf_LD a _ modeM dest _ _ imm =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
  in
  (match modeM with
   | BPF_IMM ->
     (match coq_Bpf_store (coq_Bpf_eval_l dest) (Ez_val imm) SCALAR_VALUE
              (Obj.magic sepx) propx with
      | Some p ->
        let (s', w') = p in
        Coq_inl
        (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = w'; coq_Local_list =
          localx; coq_Sep_list = s'; coq_Exist_list = exist_list })
      | None ->
        Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_operation) })
   | _ ->
     Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
       localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
       coq_Errorm = (Coq_error Invalid_operation) })

(** val coq_Bpf_LDX :
    ebpf_type assertion -> coq_SizeModifier -> coq_ModeModifier -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type
    coq_Prod_error) sum **)

let coq_Bpf_LDX a _ modeM dest src off _ =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
  in
  (match modeM with
   | BPF_MEM ->
     let w = coq_Bpf_eval_r src (Obj.magic sepx) in
     (match w with
      | Some w0 ->
        let v =
          coq_Valid_load (Vbop (Oadd, w0, (Ez_val off))) propx
            (Obj.magic sepx)
        in
        (match v with
         | Some v' ->
           (match coq_Bpf_store (coq_Bpf_eval_l dest) v' SCALAR_VALUE
                    (Obj.magic sepx) propx with
            | Some p ->
              let (s', w') = p in
              Coq_inl
              (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = w';
                coq_Local_list = localx; coq_Sep_list = s'; coq_Exist_list =
                exist_list })
            | None ->
              Coq_inr { coq_Assert_e = { coq_Prop_list = propx;
                coq_Local_list = localx; coq_Sep_list = sepx;
                coq_Exist_list = exist_list }; coq_Errorm = (Coq_error
                Invalid_operation) })
         | None ->
           Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
             localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
             coq_Errorm = (Coq_error Invalid_load) })
      | None ->
        Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_load) })
   | _ ->
     Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
       localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
       coq_Errorm = (Coq_error Invalid_operation) })

(** val coq_Bpf_ST :
    ebpf_type assertion -> coq_SizeModifier -> coq_ModeModifier -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type
    coq_Prod_error) sum **)

let coq_Bpf_ST a _ modeM dest _ off imm =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
  in
  (match modeM with
   | BPF_MEM ->
     (match coq_Bpf_mem_imm dest off imm (Obj.magic sepx) propx with
      | Some p ->
        let (s', w') = p in
        Coq_inl
        (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = w'; coq_Local_list =
          localx; coq_Sep_list = s'; coq_Exist_list = exist_list })
      | None ->
        Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_operation) })
   | _ ->
     Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
       localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
       coq_Errorm = (Coq_error Invalid_operation) })

(** val coq_Bpf_STX :
    ebpf_type assertion -> coq_SizeModifier -> coq_ModeModifier -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> (ebpf_type assertion, ebpf_type
    coq_Prod_error) sum **)

let coq_Bpf_STX a _ modeM dest src off imm =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
  in
  (match modeM with
   | BPF_MEM ->
     (match coq_Bpf_mem dest src off (Obj.magic sepx) propx with
      | Some p ->
        let (s', w') = p in
        Coq_inl
        (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = w'; coq_Local_list =
          localx; coq_Sep_list = s'; coq_Exist_list = exist_list })
      | None ->
        Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_operation) })
   | BPF_ATOMIC ->
     (match coq_Bpf_atomic dest src off (coq_Imm_to_Atomic imm)
              (Obj.magic sepx) propx with
      | Some p ->
        let (s', w') = p in
        Coq_inl
        (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = w'; coq_Local_list =
          localx; coq_Sep_list = s'; coq_Exist_list = exist_list })
      | None ->
        Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_operation) })
   | _ ->
     Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
       localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
       coq_Errorm = (Coq_error Invalid_operation) })

(** val coq_Add_new_Prop :
    coq_BPF_JMP_OP -> expr_val option -> expr_val option -> coq_Proposition
    list -> coq_Proposition option * coq_Proposition list option **)

let coq_Add_new_Prop op v w propx =
  match v with
  | Some v' ->
    (match w with
     | Some w' ->
       (match op with
        | BPF_GOTO -> (None, (Some []))
        | BPF_JEQ ->
          ((Some (Be (Pvequal, v', w'))),
            (coq_Prop_list_solver ((Be (Pvequal, v', w')) :: []) propx))
        | BPF_JGT ->
          ((Some (Be (Pvgt, v', w'))),
            (coq_Prop_list_solver ((Be (Pvgt, v', w')) :: []) propx))
        | BPF_JGE ->
          ((Some (Be (Pvge, v', w'))),
            (coq_Prop_list_solver ((Be (Pvge, v', w')) :: []) propx))
        | BPF_JSET -> (None, None)
        | BPF_JNE ->
          ((Some (Up (Be (Pvequal, v', w')))),
            (coq_Prop_list_solver ((Up (Be (Pvequal, v', w'))) :: []) propx))
        | BPF_JSGT ->
          ((Some (Be (Pvgt, v', w'))),
            (coq_Prop_list_solver ((Be (Pvgt, v', w')) :: []) propx))
        | BPF_JSGE ->
          ((Some (Be (Pvge, v', w'))),
            (coq_Prop_list_solver ((Be (Pvge, v', w')) :: []) propx))
        | BPF_JLT ->
          ((Some (Be (Pvlt, v', w'))),
            (coq_Prop_list_solver ((Be (Pvlt, v', w')) :: []) propx))
        | BPF_JSLT ->
          ((Some (Be (Pvlt, v', w'))),
            (coq_Prop_list_solver ((Be (Pvlt, v', w')) :: []) propx))
        | _ ->
          ((Some (Be (Pvle, v', w'))),
            (coq_Prop_list_solver ((Be (Pvle, v', w')) :: []) propx)))
     | None -> (None, None))
  | None -> (None, None)

(** val coq_Bpf_jmp_X :
    ebpf_type assertion -> nat -> coq_BPF_JMP_OP -> coq_Z -> coq_Z -> coq_Z
    -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list **)

let coq_Bpf_jmp_X a pc op dest src off =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
  in
  let v = coq_Bpf_eval_r dest (Obj.magic sepx) in
  let w = coq_Bpf_eval_r src (Obj.magic sepx) in
  let newP = coq_Add_new_Prop op v w propx in
  let (o, o0) = newP in
  (match o with
   | Some propx' ->
     ((Coq_inl
       (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = (propx' :: propx);
         coq_Local_list = localx; coq_Sep_list = sepx; coq_Exist_list =
         exist_list })),
       (Z.to_nat (Z.add (Z.of_nat (S pc)) off))) :: (((Coq_inl
       (coq_Union_assert coq_Ebpf_sep { coq_Prop_list =
         (app (simpl_Proposition_list ((Up propx') :: [])) propx);
         coq_Local_list = localx; coq_Sep_list = sepx; coq_Exist_list =
         exist_list })), (S pc)) :: [])
   | None ->
     (match o0 with
      | Some l ->
        (match l with
         | [] -> ((Coq_inl a), (Z.to_nat (Z.add (Z.of_nat (S pc)) off))) :: []
         | _ :: _ ->
           ((Coq_inr { coq_Assert_e = { coq_Prop_list = propx;
             coq_Local_list = localx; coq_Sep_list = sepx; coq_Exist_list =
             exist_list }; coq_Errorm = (Coq_error Invalid_operation) }), (S
             pc)) :: [])
      | None ->
        ((Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_operation) }), (S pc)) :: []))

(** val coq_Bpf_jmp_K :
    ebpf_type assertion -> nat -> coq_BPF_JMP_OP -> coq_Z -> coq_Z -> coq_Z
    -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list **)

let coq_Bpf_jmp_K a pc op dest off imm =
  let { coq_Prop_list = propx; coq_Local_list = localx; coq_Sep_list = sepx;
    coq_Exist_list = exist_list } = coq_Split_assert coq_Ebpf_sep a
  in
  let v = coq_Bpf_eval_r dest (Obj.magic sepx) in
  let w = Some (Ez_val imm) in
  let newP = coq_Add_new_Prop op v w propx in
  let (o, o0) = newP in
  (match o with
   | Some propx' ->
     ((Coq_inl
       (coq_Union_assert coq_Ebpf_sep { coq_Prop_list = (propx' :: propx);
         coq_Local_list = localx; coq_Sep_list = sepx; coq_Exist_list =
         exist_list })),
       (Z.to_nat (Z.add (Z.of_nat (S pc)) off))) :: (((Coq_inl
       (coq_Union_assert coq_Ebpf_sep { coq_Prop_list =
         (app (simpl_Proposition_list ((Up propx') :: [])) propx);
         coq_Local_list = localx; coq_Sep_list = sepx; coq_Exist_list =
         exist_list })), (S pc)) :: [])
   | None ->
     (match o0 with
      | Some l ->
        (match l with
         | [] -> ((Coq_inl a), (Z.to_nat (Z.add (Z.of_nat (S pc)) off))) :: []
         | _ :: _ ->
           ((Coq_inr { coq_Assert_e = { coq_Prop_list = propx;
             coq_Local_list = localx; coq_Sep_list = sepx; coq_Exist_list =
             exist_list }; coq_Errorm = (Coq_error Invalid_operation) }), (S
             pc)) :: [])
      | None ->
        ((Coq_inr { coq_Assert_e = { coq_Prop_list = propx; coq_Local_list =
          localx; coq_Sep_list = sepx; coq_Exist_list = exist_list };
          coq_Errorm = (Coq_error Invalid_operation) }), (S pc)) :: []))

(** val coq_Bpf_jmp :
    ebpf_type assertion -> nat -> coq_BPF_JMP_OP -> coq_OpSource -> coq_Z ->
    coq_Z -> coq_Z -> coq_Z -> ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum * nat) list **)

let coq_Bpf_jmp a pc op os dest src off imm =
  match os with
  | BPF_X -> coq_Bpf_jmp_X a pc op dest src off
  | BPF_K -> coq_Bpf_jmp_K a pc op dest off imm

(** val coq_Assertion_ignore_return :
    ebpf_type coq_Prod_ret list -> nat -> ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum * nat) list **)

let coq_Assertion_ignore_return p pc =
  map (fun a -> ((Coq_inl (coq_Union_assert coq_Ebpf_sep a.coq_Assert_r)), (S
    pc))) p

(** val coq_Bpf_call_num :
    ebpf_type assertion -> ebpf_type funcdes list -> expr_val option list ->
    nat -> ((ebpf_type assertion, ebpf_type coq_Prod_error) sum * nat) list **)

let coq_Bpf_call_num a func arg pc =
  coq_Assertion_ignore_return
    (coq_Change_for_func_list coq_Ebpf_sep (coq_Split_assert coq_Ebpf_sep a)
      func arg) pc

(** val coq_Find_id_in_Helper :
    (nat * ebpf_type funcspec) list -> ident -> (nat * ebpf_type funcdes
    list) option **)

let rec coq_Find_id_in_Helper helper id =
  match helper with
  | [] -> None
  | p :: l' ->
    let (num, f) = p in
    let (id', func) = f in
    if Pos.eqb id' id then Some (num, func) else coq_Find_id_in_Helper l' id

(** val eval_arg : ebpf_type assertion -> nat -> expr_val option list **)

let rec eval_arg a num = match num with
| O -> []
| S n' -> app (eval_arg a n') ((Some (coq_Bpf_eval_l (Z.of_nat num))) :: [])

(** val coq_Bpf_call :
    ebpf_type assertion -> nat -> ident -> ((ebpf_type assertion, ebpf_type
    coq_Prod_error) sum * nat) list **)

let coq_Bpf_call a pc id =
  match coq_Find_id_in_Helper coq_Helper_func id with
  | Some p ->
    let (num, func) = p in
    let b = coq_Bpf_call_num a func (eval_arg a num) pc in
    (match b with
     | [] ->
       ((Coq_inr { coq_Assert_e = (coq_Split_assert coq_Ebpf_sep a);
         coq_Errorm = (Coq_error Invalid_call) }), inf) :: []
     | _ :: _ -> b)
  | None ->
    ((Coq_inr { coq_Assert_e = (coq_Split_assert coq_Ebpf_sep a);
      coq_Errorm = (Coq_error Nofuncdef) }), inf) :: []
