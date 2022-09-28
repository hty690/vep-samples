(* This file contain the symbolic execution of Ebpf programs*)

From Ebpf Require Import Ebpfstatement Ebpfstate Ebpfop.

Fixpoint look_up (n : nat) (s : list Singleton_statement) : option Singleton_statement :=
  match s with
    | nil => None
    | a :: s' => match n with
                  | O => Some a
                  | S n' => look_up n' s'
                 end
  end. 

Fixpoint Remove_B_in_ProdAB {A B : Type} (eqbB : B -> B -> bool) (l : list (A * B)) (b : B) : list (A * B) :=
  match l with
    | nil => nil 
    | (a , b') :: l' => if (eqbB b b') then Remove_B_in_ProdAB eqbB l' b
                        else (a , b') :: Remove_B_in_ProdAB eqbB l' b
  end.

Definition look_up_list_assertion (n : nat) (l : list (list assertion * nat)) : option (list assertion) :=
  Find_B_in_prodAB Nat.eqb l n.

Definition option_to_list {A : Type} (a : option (list  A)) : list A :=
  match a with
    | None => nil
    | Some v => v
  end.

Fixpoint Eval_from_Real (n : nat) (l : list (list assertion * nat)) : list (list (assertion + Prod_error) * nat) :=
  (map (fun v => inl v) (option_to_list (look_up_list_assertion n l)) , n) ::
  match n with
    | O => nil
    | S n' => Eval_from_Real n' l
  end.

Definition LD_exec (a : assertion) (pc : nat) (BLD : BPF_LD) (dest src off imm : Z) := 
  match BLD with
    | LD SizeM ModeM =>  (Bpf_LD a SizeM ModeM dest src off imm, S pc)
  end.

Definition LDX_exec (a : assertion) (pc : nat) (BLDX : BPF_LDX) (dest src off imm : Z) := 
  match BLDX with
    | LDX SizeM ModeM =>  (Bpf_LDX a SizeM ModeM dest src off imm, S pc)
  end.

Definition ST_exec (a : assertion) (pc : nat) (BST : BPF_ST) (dest src off imm : Z) :=
  match BST with
    | ST SizeM ModeM =>  (Bpf_ST a SizeM ModeM dest src off imm, S pc)
  end.

Definition STX_exec (a : assertion) (pc : nat) (BSTX : BPF_STX) (dest src off imm : Z) := 
  match BSTX with
    | STX SizeM ModeM =>  (Bpf_STX a SizeM ModeM dest src off imm, S pc)
  end.

Definition JMP_exec (a : assertion) (pc : nat) (BJMP : BPF_JMP) (os : OpSource) (dest src off imm : Z) := 
  match BJMP with 
    | BPF_EXIT => let (Propx , Localx , Sepx, Exist_list) := Split_assert a in
                    match (Bpf_eval_r 0 Sepx) with
                      | None => (inr (mk_error _ _ (mk_A _ _ Propx Localx Sepx Exist_list) (error Invalid_operation)) , inf)  
                      | Some w => (inl (Union_assert (mk_A _ _ Propx Localx Sepx Exist_list)) , S pc)
                    end :: nil
    | BPF_CALL => Bpf_call a pc (Z.to_pos imm) 
    | JMP op => Bpf_jmp a pc op os dest src off imm
  end.

Definition JMP32_exec (a : assertion) (pc : nat) (BJMP32 : BPF_JMP32) (os : OpSource) (dest src off imm : Z) := 
  match BJMP32 with 
    | JMP32 op => Bpf_jmp a pc op os dest src off imm
  end.

Definition ALU_exec (a : assertion) (pc : nat) (BALU : BPF_ALU) (os : OpSource) (dest src off imm : Z) := 
  match BALU with
    | ALU op =>  match os with
                    | BPF_K => (Bpf_op_K a op dest imm, S pc)
                    | BPF_X => (Bpf_op_X a op dest src, S pc)
                   end
  end.

Definition ALU64_exec (a : assertion) (pc : nat) (BALU64 : BPF_ALU64) (os : OpSource) (dest src off imm : Z) := 
  match BALU64 with
    | ALU64 op => match os with
                    | BPF_K => (Bpf_op_K a op dest imm, S pc)
                    | BPF_X => (Bpf_op_X a op dest src, S pc)
                  end
  end.

Definition Singleton_exec (a : assertion * nat) (sl : list Singleton_statement) :=  
  let s := look_up (snd a) sl in 
    match s with
      | None => nil
      | Some (Empty_line) => (inl (fst a) , S (snd a)) :: nil
      | Some (Op0 BLD dest src off imm) => LD_exec (fst a) (snd a) BLD dest src off imm :: nil 
      | Some (Op1 BLDX dest src off imm) => LDX_exec (fst a) (snd a) BLDX dest src off imm :: nil
      | Some (Op2 BST dest src off imm) => ST_exec (fst a) (snd a) BST dest src off imm :: nil
      | Some (Op3 BSTX dest src off imm) => STX_exec (fst a) (snd a) BSTX dest src off imm :: nil
      | Some (Op4 BALU os dest src off imm) => ALU_exec (fst a) (snd a) BALU os dest src off imm :: nil
      | Some (Op5 BJMP os dest src off imm) => JMP_exec (fst a) (snd a) BJMP os dest src off imm
      | Some (Op6 BJMP32 os dest src off imm) => JMP32_exec (fst a) (snd a) BJMP32 os dest src off imm
      | Some (Op7 BALU64 os dest src off imm) => ALU64_exec (fst a) (snd a) BALU64 os dest src off imm :: nil
    end.

Fixpoint Singleton_exec_list (a : list (assertion + Prod_error)) (pc : nat) (s : list Singleton_statement) :=
  match a with
    | nil => nil
    | a' :: l =>  match a' with 
                    | inl a0 => (Singleton_exec (a0 , pc) s)
                    | inr a0 => (a' , inf) :: nil
                  end ++ Singleton_exec_list l pc s
  end. 

Definition Update_eqb (a b : assertion + Prod_error) : bool :=
  match a , b with
    | inl a' , inl b' => Assertion_eqb a' b'
    | inr e1 , inr e2 => eqb_Prod_error e1 e2
    | _ , _ => false
  end.

Definition update (l : list (list (assertion + Prod_error) * nat)) (a : assertion + Prod_error) (pc : nat) :=
  match Find_B_in_prodAB Nat.eqb l pc with  
    | None => (a :: nil, pc) :: l 
    | Some a' => (a :: a' , pc) :: (Remove_B_in_ProdAB Nat.eqb l pc)
  end.

Definition list_update (l : list (list (assertion + Prod_error) * nat)) (a : list (assertion + Prod_error)) (pc : nat) :=
  match Find_B_in_prodAB Nat.eqb l pc with  
    | None => (a , pc) :: l 
    | Some a' => (a ++ a' , pc) :: (Remove_B_in_ProdAB Nat.eqb l pc)
  end.

Fixpoint update_list (l : list ((list (assertion + Prod_error)) * nat)) (l2 : list ((assertion + Prod_error) * nat)) :=
  match l2 with  
    | nil => l
    | (a , pc) :: l' => update (update_list l l') a pc
  end.

Fixpoint list_update_list (l : list ((list (assertion + Prod_error)) * nat)) (l2 : list (list (assertion + Prod_error) * nat)) :=
  match l2 with  
    | nil => l
    | (a , pc) :: l' => list_update (list_update_list l l') a pc
  end.

Record Divide_assertion : Type := mk_div {
  Front_part : list ((assertion + Prod_error) * nat);
  Back_part : list ((assertion + Prod_error) * nat);
  No_need : list ((assertion + Prod_error) * nat);
}.

Fixpoint Divide (l : list ((assertion + Prod_error) * nat)) (pc : nat) (Real_tag : list (list assertion * nat)) :=
  match l with
    | nil => mk_div nil nil nil
    | (a , pc') :: l' => let res := Divide l' pc Real_tag in 
                          match (Find_B_in_prodAB Nat.eqb Real_tag pc') with
                            | None => if (Nat.ltb pc' pc) then mk_div ((a , pc'):: (Front_part res)) (Back_part res) (No_need res)
                                      else mk_div (Front_part res) ((a,pc') :: Back_part res) (No_need res)
                            | Some _ => mk_div (Front_part res) (Back_part res) ((a,pc') :: No_need res)
                          end  
                             
  end.

Fixpoint Program_exec_once (time pc: nat) (l Front_assertion : list ((list (assertion + Prod_error)) * nat)) (s : list Singleton_statement) (Real_tag : list (list assertion * nat)):=
  match time with
    | O => (nil , nil) 
    | S n' => let need_to_exec := match Find_B_in_prodAB Nat.eqb l pc , Find_B_in_prodAB Nat.eqb Front_assertion pc with
                                   | None , None => nil
                                   | Some a , None => a 
                                   | None , Some a => a 
                                   | Some a , Some b => a ++ b
                                  end in
              let Results := Singleton_exec_list need_to_exec pc s in 
              let res := Divide Results pc Real_tag in
              let (Virtual_tags , Next_round) := Program_exec_once n' (pc + 1) l (update_list Front_assertion (Front_part res)) s Real_tag in
                (update_list Virtual_tags (No_need res) , update_list Next_round (Back_part res)) 
  end. 

Fixpoint Program_exec_several (l : list ((list (assertion + Prod_error)) * nat)) (s : list Singleton_statement) (Real_list : list (list assertion * nat)) (time len: nat) :=
  match time with 
    | O => l
    | S n' => let (Virtual_tag, Next_round) := Program_exec_once len O l nil s Real_list in
              let Virtual_tags := list_update_list Virtual_tag l in  
                match Next_round with
                  | nil => Virtual_tags 
                  | _ => list_update_list Virtual_tags (Program_exec_several Next_round s Real_list n' len)
                end 
  end.

Fixpoint Program_checker (Real_tag : list (list assertion * nat)) (Virtual_tag : list (list (assertion + Prod_error) * nat)) := 
  match Virtual_tag with 
    | nil => nil
    | (a , pc) :: l' => match (look_up_list_assertion pc Real_tag) with
                          | None => Program_checker Real_tag l'
                          | Some v => (a, v) :: Program_checker Real_tag l'
                        end
  end.

Fixpoint No_error (l : list (assertion + Prod_error)) : bool :=
  match l with
    | nil => true
    | a :: l' => match a with
                  | inr _ => false
                  | _ => No_error l'
                 end 
  end.
  
Fixpoint Check_assertion (l : list (list (assertion + Prod_error) * list assertion)) : bool * list (list (assertion + Prod_error) * list assertion) :=
  match l with
    | nil => (true , nil)  
    | (s , v) :: l' => let (flag , ls) := Check_assertion l' in 
                       if (No_error s && Ass_list_solver v (Clear_Prod_error s)) then (flag, ls) 
                       else (false , (s , v) :: ls)
  end.

Definition Program_exec (Real_tag : list (list assertion * nat)) (s : list Singleton_statement) :=
  let n := length s in 
    let Virtual_tag := Program_exec_several (map (fun a => (map (fun b => inl b) (fst a) , snd a)) Real_tag) s Real_tag n n in
      Check_assertion (Program_checker Real_tag Virtual_tag).

Fixpoint No_error_all (After_exec : list (list (assertion + Prod_error) * nat)) :=
  match After_exec with
    | nil => (true, nil)
    | (a , pc) :: l' => if (No_error a) then No_error_all l' else (false , pc :: snd (No_error_all l') )
  end.