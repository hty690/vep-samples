(* This file contain the symbolic execution of a single instruction*)

From Ebpf Require Import Ebpfstatement Ebpfstate Ebpf_helper.


Definition Bpf_eval_l (reg : Z) := V_vari (Z.to_pos (reg + 1)).

Fixpoint Bpf_eval_val_rec (x : expr_val) (l : list Ebpf_Sep) :=
  match l with
    | nil => None
    | h :: s' => match h with
                  | @Data_at _ _ x1 ty x2 => if ((eqb_val x2 x) && (negb (eqb_ptype ty NOT_INIT))) then 
                                             match Bpf_eval_val_rec x s' with
                                               | None => Some x1
                                               | Some x' => Some x'
                                             end 
                                             else Bpf_eval_val_rec x s'
                  | @Memory _ _ x1 _ s0 => if x1 =? x then Some s0 else Bpf_eval_val_rec x s'
                  | @Other _ _ (Ebpf_context x1 st ed s0) => if (x =? Simplify_val (Vbop Oadd x1 st)) then Some Context_list
                                                    else if (x =? Simplify_val (Vbop Oadd x1 ed)) then Some data_ed
                                                    else if (x =? x1) then Some s0
                                                    else Bpf_eval_val_rec x s'
                  | _ => Bpf_eval_val_rec x s'
                 end
  end.
 
Definition Bpf_eval_val (x : expr_val) (l : list Ebpf_Sep) :=
  Bpf_eval_val_rec (Simplify_val x) l.

Definition Bpf_eval_r (reg : Z) (Sepx : list Ebpf_Sep) := 
  Bpf_eval_val (Bpf_eval_l reg) Sepx.  

Definition Valid_load (x : expr_val) (Propx : list Proposition) (Sepx : list Ebpf_Sep) :=
  Some (Simplify_val x).
 (* match Simplify_val x with
    | Vlop Vnth pos l => match Find_in_Sep_list_val l Sepx with 
                          | Some (@Memory _ _ x1 Maxsize s0) => 
                             match Prop_list_solver (In_bound (Simplify_val (Vbop Osub (Ez_val 0) Maxsize)) pos (Ez_val 0) :: nil) Propx with
                               | Some nil => Some (Simplify_val x)
                               | _ => None 
                             end  
                          | Some (@Other _ _ (Map_element map_name key x1 size)) => 
                             match Prop_list_solver (In_bound (Ez_val 0) pos size :: nil) Propx with
                               | Some nil => Some (Simplify_val x)
                               | _ => None 
                             end 
                          | _ => None
                         end
    | Ez_val x' => Some (Ez_val x') 
    | _ => None
  end. 
  
  Here need prop_solver to support
  
  *)

Definition Change_prop (Sepx : list Ebpf_Sep) (Propx : list Proposition) (reg x : expr_val) := 
  if (Find_list_val reg) then Some (Sepx, Change_prop_vequal Propx reg x) 
  else if (Used_in_val reg (V_vari 13%positive)) then Some (Sepx, Change_prop_vequal Propx reg x)
  else None.

Definition Bpf_store (reg x : expr_val) (ty : ebpf_type) (Sepx : list Ebpf_Sep) (Propx : list Proposition) := 
  match (Find_in_Sep_list_address reg Sepx) with
    | None => Change_prop Sepx Propx reg x
    | Some w => match w with 
                  | Other _ _ (Ebpf_context _ _ _ _ ) => Some (Sepx , Propx)
                  | _ => let (Ns, flag) := Change_sepx_typed Sepx reg x ty in
                          if (flag) then Some (Ns , Propx)
                          else None
                end
  end.

Definition Bpf_op (op : BPF_OP) (x1 x2 : option expr_val) : option expr_val := 
  match x1, x2 with 
    | Some v1, Some v2 => 
        match op with
        | BPF_ADD => Some (Simplify_val (Vbop Oadd v1 v2))
        | BPF_SUB => Some (Simplify_val (Vbop Osub v1 v2))
        | BPF_MUL => Some (Simplify_val (Vbop Omul v1 v2))
        | BPF_DIV => Some (Simplify_val (Vbop Odiv v1 v2))
        | BPF_OR  => Some (Simplify_val (Vbop Oor v1 v2))
        | BPF_AND => Some (Simplify_val (Vbop Oand v1 v2)) 
        | BPF_LSH => Some (Simplify_val (Vbop Oshl v1 v2))
        | BPF_RSH => Some (Simplify_val (Vbop Oshr v1 v2))
        | BPF_NEG => Some (Simplify_val (Vuop Oneg v1))
        | BPF_MOD => Some (Simplify_val (Vbop Omod v1 v2))
        | BPF_XOR => Some (Simplify_val (Vbop Oxor v1 v2))
        | BPF_MOV => x2
        | BPF_ARSH => Some (Simplify_val (Vbop OAshl v1 v2)) (* sign extending shift right *)
        | BPF_END => Some (Simplify_val (Vbop Oend v1 v2)) (* endianness conversion*)
        end
    | None , Some v2 => match op with  
                          | BPF_MOV => x2
                          | _ => None
                        end
    | _ , _ => None
  end.


Definition Bpf_op_K (a : assertion) (op : BPF_OP) (dest imm : Z) :=
  let (Propx , Localx , Sepx, Exist_list) := Split_assert a in
  match (Bpf_op op (Bpf_eval_r dest Sepx) (Some (Ez_val imm))) with
    | None => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))  
    | Some w => match (Bpf_store (Bpf_eval_l dest) w SCALAR_VALUE Sepx Propx) with
                  | None => inr (mk_error _ _  (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                  | Some (s', w')  => inl (Union_assert (mk_A _ _ w' Localx s' Exist_list))
                end
  end.

Definition Bpf_op_X (a : assertion) (op : BPF_OP) (dest src : Z) :=
  let (Propx , Localx , Sepx, Exist_list) := Split_assert a in
    match (Bpf_op op (Bpf_eval_r dest Sepx) (Bpf_eval_r src Sepx)) with
      | None => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
      | Some w => match (Bpf_store (Bpf_eval_l dest) w SCALAR_VALUE Sepx Propx) with
                    | None => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                    | Some (s',w') => inl (Union_assert (mk_A _ _ w' Localx s' Exist_list))
                  end
    end.
  
Definition Bpf_mem (dest src off : Z) (Sepx : list (Ebpf_Sep)) (Propx : list Proposition) := 
  match (Bpf_eval_r dest Sepx) , (Bpf_eval_r src Sepx) with
    | Some v1 , Some v2 => let v := Valid_load (Vbop Oadd v1 (Ez_val off)) Propx Sepx in 
                           match v with
                             | None => None 
                             | Some v' => Bpf_store (Simplify_val (Vbop Oadd v' (Ez_val off))) v2 SCALAR_VALUE Sepx Propx
                           end
    | _ , _ => None
  end.

Definition Bpf_mem_imm (dest off imm : Z) (Sepx : list (Ebpf_Sep))(Propx : list Proposition)  := 
  match (Bpf_eval_r dest Sepx) with
    | Some v1 => let v := Valid_load (Vbop Oadd v1 (Ez_val off)) Propx Sepx in 
                  match v with
                    | None => None 
                    | Some v' => Bpf_store (Simplify_val (Vbop Oadd v' (Ez_val off))) (Ez_val imm) SCALAR_VALUE Sepx Propx
                  end
    | None => None
  end.

Definition Bpf_atomic (dest src off : Z) (op : option Atomic) (Sepx : list (Ebpf_Sep))(Propx : list Proposition) :=
  match op with
    | None => None
    | Some op' => match op' with
                    | BPF_ADDA => match (Bpf_op BPF_ADD (Bpf_eval_val (Simplify_val (Vbop Oadd (Bpf_eval_l dest) (Ez_val off))) Sepx) (Bpf_eval_r src Sepx)) with
                                  | None => None   
                                  | Some w => Bpf_store (Simplify_val (Vbop Oadd (Bpf_eval_l dest) (Ez_val off))) w SCALAR_VALUE Sepx Propx
                                  end
                    | BPF_ANDA => None
                    | BPF_ORA => None
                    | BPF_XORA => None
                    | BPF_ADDAF => None
                    | BPF_ANDAF => None
                    | BPF_ORAF => None
                    | BPF_XORAF => None
                    | BPF_XCHGA => None
                    | BPF_CMPXCHGA => None
                  end
  end.

Definition Bpf_LD (a : assertion) (SizeM : SizeModifier) (ModeM : ModeModifier) (dest src off imm : Z) : assertion + Prod_error :=
  let (Propx, Localx, Sepx, Exist_list) := Split_assert a in
    match ModeM with 
      | BPF_ABS => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                   (* R0 = *(uint * ) (skb->data + imm32) *)
      | BPF_IND => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                    (* R0 = ntohl( * (u32 * ) (((struct sk_buff * ) R6)->data + src_reg + imm32)) *)
      | BPF_IMM => match (Bpf_store (Bpf_eval_l dest) (Ez_val imm) SCALAR_VALUE Sepx Propx) with
                      | None => inr (mk_error _ _  (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                      | Some (s',w') => inl (Union_assert (mk_A _ _ w' Localx s' Exist_list))
                   end
                   (* dst_reg = imm *)
      | _ => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
    end.

Definition Bpf_LDX (a : assertion) (SizeM : SizeModifier) (ModeM : ModeModifier) (dest src off imm : Z) :=
  let (Propx , Localx , Sepx, Exist_list) := Split_assert a in
  match ModeM with 
    | BPF_MEM => (* dst_reg = *(uint * ) (src_reg + off16) *)
      let w := Bpf_eval_r src Sepx in 
       match w with
        | None => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_load))
        | Some w0 =>
      let v := Valid_load (Vbop Oadd w0 (Ez_val off)) Propx Sepx in
      match v with
        | None => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_load))
        | Some v' => match (Bpf_store (Bpf_eval_l dest) v' SCALAR_VALUE Sepx Propx) with
                      | None => inr (mk_error _ _  (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                      | Some (s',w') => inl (Union_assert (mk_A _ _ w' Localx s' Exist_list))
                     end
      end
      end
    | _ => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
  end.

Definition Bpf_ST (a : assertion) (SizeM : SizeModifier) (ModeM : ModeModifier) (dest src off imm : Z) :=
  let (Propx , Localx , Sepx, Exist_list) := Split_assert a in
  match ModeM with 
    | BPF_MEM => match (Bpf_mem_imm dest off imm Sepx Propx) with
                   | None => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                   | Some (s',w') => inl (Union_assert (mk_A _ _ w' Localx s' Exist_list))
                 end
                 (* *(uint * ) (dst_reg + off16) = imm32 *)
    | _ => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
  end.  

 Definition Bpf_STX (a : assertion) (SizeM : SizeModifier) (ModeM : ModeModifier) (dest src off imm : Z) :=
  let (Propx , Localx , Sepx, Exist_list) := Split_assert a in 
    match ModeM with 
      | BPF_MEM => match (Bpf_mem dest src off Sepx Propx) with
                     | None => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                     | Some (s',w') => inl (Union_assert (mk_A _ _ w' Localx s' Exist_list))
                   end
                      (* *(uint * ) (dst_reg + off16) = src_reg *)
      | BPF_ATOMIC => match (Bpf_atomic dest src off (Imm_to_Atomic imm) Sepx Propx) with
                        | None => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
                        | Some (s',w') => inl (Union_assert (mk_A _ _ w' Localx s' Exist_list))
                      end
      | _ => inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation))
    end. 

Definition Add_new_Prop (op : BPF_JMP_OP) (v w : option expr_val) (Propx : list Proposition) : option Proposition * option (list Proposition) :=
  match v , w with
    | Some v' , Some w' => match op with 
                          | BPF_GOTO => (None , Some nil)
                          | BPF_JEQ => (Some (Be Pvequal v' w') , Prop_list_solver (Be Pvequal v' w' :: nil) Propx)
                          | BPF_JGT => (Some (Be Pvgt v' w') , Prop_list_solver (Be Pvgt v' w' :: nil) Propx)
                          | BPF_JGE => (Some (Be Pvge v' w') , Prop_list_solver (Be Pvge v' w' :: nil)  Propx)
                          | BPF_JNE => (Some (Up Pnot (Be Pvequal v' w')) , Prop_list_solver (Up Pnot (Be Pvequal v' w') :: nil) Propx)
                          | BPF_JLT => (Some (Be Pvlt v' w') , Prop_list_solver (Be Pvlt v' w' :: nil) Propx)
                          | BPF_JLE => (Some (Be Pvle v' w') , Prop_list_solver (Be Pvle v' w' :: nil) Propx)
                          | BPF_JSET => (None , None)
                          | BPF_JSGT => (Some (Be Pvgt v' w') , Prop_list_solver (Be Pvgt v' w' :: nil) Propx)
                          | BPF_JSGE => (Some (Be Pvge v' w') , Prop_list_solver (Be Pvge v' w' :: nil) Propx)
                          | BPF_JSLT => (Some (Be Pvlt v' w') , Prop_list_solver (Be Pvlt v' w' :: nil) Propx)
                          | BPF_JSLE => (Some (Be Pvle v' w') , Prop_list_solver (Be Pvle v' w' :: nil) Propx)
                          end
    | _ , _ => (None , None)
  end. 

Definition Bpf_jmp_X (a : assertion) (pc : nat) (op : BPF_JMP_OP) (dest src off : Z) := 
  let (Propx , Localx , Sepx, Exist_list) := Split_assert a in
  let v := (Bpf_eval_r dest Sepx) in
  let w := (Bpf_eval_r src Sepx) in
  let NewP := Add_new_Prop op v w Propx in 
    match NewP with
      | (None , Some nil) => (inl a , Z.to_nat (Z.of_nat (S pc) + off)) :: nil
      | (None , _)  => (inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation)) , S pc) :: nil
      | (Some Propx' , _) => (inl (Union_assert (mk_A _ _ (Propx' :: Propx) Localx Sepx Exist_list)),  Z.to_nat (Z.of_nat (S pc) + off)) :: 
      (inl (Union_assert (mk_A _ _ ((simpl_Proposition_list (Up Pnot Propx' :: nil)) ++ Propx) Localx Sepx Exist_list)) ,S pc) :: nil
    end. 

Definition Bpf_jmp_K (a : assertion) (pc : nat) (op : BPF_JMP_OP) (dest off imm : Z) := 
  let (Propx , Localx , Sepx, Exist_list) := Split_assert a in
  let v := (Bpf_eval_r dest Sepx) in
  let w := Some (Ez_val imm) in
  let NewP := Add_new_Prop op v w Propx in 
    match NewP with
      | (None , Some nil) => (inl a , Z.to_nat (Z.of_nat (S pc) + off)) :: nil
      | (None , _)  => (inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation)) , S pc) :: nil
      | (Some Propx' , _) => (inl (Union_assert (mk_A _ _ (Propx' :: Propx) Localx Sepx Exist_list)),  Z.to_nat (Z.of_nat (S pc) + off)) :: 
      (inl (Union_assert (mk_A _ _ ((simpl_Proposition_list (Up Pnot Propx' :: nil)) ++ Propx) Localx Sepx Exist_list)) ,S pc) :: nil
    end.  

Definition Bpf_jmp (a : assertion) (pc : nat) (op : BPF_JMP_OP) (os : OpSource) (dest src off imm : Z) := 
  match os with
    | BPF_K => Bpf_jmp_K a pc op dest off imm
    | BPF_X => Bpf_jmp_X a pc op dest src off
  end.

Definition Assertion_ignore_return (P : list Prod_ret) (pc : nat) : list ((assertion + Prod_error) * nat) :=
  map (fun a => (inl (Union_assert a.(Assert_r)) , S pc)) P.

Definition Bpf_call_num (a : assertion) (func : list funcdes) (arg : list (option expr_val)) (pc : nat) :=
  Assertion_ignore_return (Change_for_func_list (Split_assert a) func arg) pc.

Fixpoint Find_id_in_Helper (Helper : list (nat * funcspec)) (id : ident) :=
  match Helper with
    | nil => None
    | (num , (id' , func)) :: l' => if (Pos.eqb id' id) then Some (num , func) else Find_id_in_Helper l' id
  end.

Fixpoint eval_arg (a : assertion) (num : nat) : list (option expr_val):=
  match num with 
    | O => nil 
    | S n' => eval_arg a n' ++ (Some (Bpf_eval_l (Z.of_nat num))) :: nil
  end.

Definition Bpf_call (a : assertion) (pc : nat) (id : ident) :=
  match Find_id_in_Helper Helper_func id with
    | None => (inr (mk_error _ _ (Split_assert a) (error Nofuncdef)) , inf) :: nil
    | Some (num , func) => let b := Bpf_call_num a func (eval_arg a num) pc in
                             match b with
                              | nil => (inr (mk_error _ _ (Split_assert a) (error Invalid_call)) , inf) :: nil
                              | _ => b
                             end
  end.