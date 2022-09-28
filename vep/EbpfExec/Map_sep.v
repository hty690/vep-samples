(** This file contains the definition of Ebpf Sep predicate, especially for Ebpf Map *)
From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstatement.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Inductive ebpf_type : Type :=
  | NOT_INIT : ebpf_type (* the register has not been written to *)
  | SCALAR_VALUE : ebpf_type (* some value which is not usable as a pointer *)
  | PTR_TO_CTX : ebpf_type (* Pointer to bpf_context *)
  | CONST_PTR_TO_MAP : ebpf_type (* Pointer to struct bpf_map. “Const” because arithmetic on these pointers is forbidden. *)
  | PTR_TO_MAP_VALUE : ebpf_type (* Pointer to the value stored in a map element *)
  | PTR_TO_MAP_VALUE_OR_NULL : ebpf_type (* Either a pointer to a map value, or NULL , which becomes a PTR_TO_MAP_VALUE when checked != NULL *)
  | PTR_TO_STACK : ebpf_type (* Frame pointer *)
  | PTR_TO_PACKET : ebpf_type (* skb->data *)
  | PTR_TO_PACKET_END : ebpf_type (* skb->data + headlen; arithmetic forbidden *)
  | PTR_TO_SOCKET : ebpf_type (* Pointer to struct bpf_sock_ops, implicitly refcounted *)
  | PTR_TO_SOCKET_OR_NULL : ebpf_type (* Either a pointer to a socket, or NULL, which becomes a PTR_TO_SOCKET when checked != NULL *)
.
Definition eqb_ebpf (a b : ebpf_type) := 
  match a , b with
    | NOT_INIT , NOT_INIT => true
    | SCALAR_VALUE , SCALAR_VALUE => true
    | PTR_TO_CTX , PTR_TO_CTX => true
    | CONST_PTR_TO_MAP , CONST_PTR_TO_MAP => true
    | PTR_TO_MAP_VALUE , PTR_TO_MAP_VALUE => true
    | PTR_TO_MAP_VALUE_OR_NULL , PTR_TO_MAP_VALUE_OR_NULL => true
    | PTR_TO_STACK , PTR_TO_STACK => true
    | PTR_TO_PACKET , PTR_TO_PACKET => true
    | PTR_TO_PACKET_END , PTR_TO_PACKET_END => true
    | PTR_TO_SOCKET , PTR_TO_SOCKET => true
    | PTR_TO_SOCKET_OR_NULL , PTR_TO_SOCKET_OR_NULL => true
    | _ , _ => false
  end.

Definition is_pointer_type (a : ebpf_type) :=
  match a with
    | PTR_TO_CTX  
    | CONST_PTR_TO_MAP 
    | PTR_TO_MAP_VALUE 
    | PTR_TO_MAP_VALUE_OR_NULL 
    | PTR_TO_STACK 
    | PTR_TO_PACKET 
    | PTR_TO_PACKET_END 
    | PTR_TO_SOCKET 
    | PTR_TO_SOCKET_OR_NULL => true
    | _ => false
  end.



(** Map type and its corresponding ident
Generic maps :

BPF_MAP_TYPE_HASH                                 1 
BPF_MAP_TYPE_ARRAY                                2 
BPF_MAP_TYPE_PERCPU_HASH                          3
BPF_MAP_TYPE_PERCPU_ARRAY                         4
BPF_MAP_TYPE_LRU_HASH                             5
BPF_MAP_TYPE_LRU_PERCPU_HASH                      6
BPF_MAP_TYPE_LPM_TRIE                             7

Non-generic maps :

BPF_MAP_TYPE_PROG_ARRAY                           8
BPF_MAP_TYPE_PERF_EVENT_ARRAY                     9
BPF_MAP_TYPE_CGROUP_ARRAY                         10
BPF_MAP_TYPE_STACK_TRACE                          11
BPF_MAP_TYPE_ARRAY_OF_MAPS                        12
BPF_MAP_TYPE_HASH_OF_MAPS                         13

*)

(** Flags for maps
  BPF_ANY		= 0, /* create new element or update existing */
	BPF_NOEXIST	= 1, /* create new element if it didn't exist */
	BPF_EXIST	= 2, /* update existing element */
	BPF_F_LOCK	= 4, /* spin_lock-ed map_lookup/map_update */
*)



(** Context type,  data_start position , data_end position

__sk_buff 76 80      



*)

Inductive Ebpf_t : Type :=
  | Ebpf_map : expr_val -> expr_val -> expr_val -> expr_val -> expr_val -> Ebpf_t   (* Map name, type_id, key_length, value_length, map_size *)
  | Ebpf_context : expr_val -> expr_val -> expr_val -> expr_val -> Ebpf_t (* package name, data_start, data_end, value list *)
  | Accessable : expr_val -> expr_val -> Ebpf_t (** You can access the memory of length len starting from x*)
  | Map_element : expr_val -> expr_val -> expr_val -> expr_val -> Ebpf_t (** map name, key , value & length , regard it as a memory block *)
.

(* look_up (key)  :  return value *)

(* update (key , value) : ok *)

(* delete (key) : ok *)

Inductive Ebpf_rule : Type :=
  .

Definition Ebpf_e := expr_val.

#[export] Instance Ebpf_sep : Separation_def ebpf_type := {

T := Ebpf_t;
rule := Ebpf_rule;
eqb_ptype := eqb_ebpf;
E := Ebpf_e; (* expression for check *)
eqb_E := eqb_val;

eqb_sep t1 t2 :=
  match t1, t2 with
  | Ebpf_map x id key value size, Ebpf_map x' id' key' value' size' => eqb_val x x' && eqb_val id id' && eqb_val key key' && eqb_val value value' && eqb_val size size'  
  | Ebpf_context x data_start data_end l, Ebpf_context x1 data_start' data_end' l' => eqb_val x x1 && eqb_val data_start data_start' && eqb_val data_end data_end' && eqb_val l l'
  | Accessable x len , Accessable y len' => eqb_val x y && eqb_val len len'
  | Map_element map_name key x size , Map_element map_name' key' x' size' => eqb_val map_name map_name' && eqb_val key key' &&  eqb_val x x' && eqb_val size size' 
  | _ , _ => false
  end;

Use_in_other t V :=
  match t with
  | Ebpf_map x id key value size => eqb_val V x || eqb_val V id || eqb_val V key || eqb_val V value || eqb_val V size 
  | Ebpf_context x data_start data_end l => eqb_val V x || eqb_val V data_start || eqb_val V data_end || eqb_val V l
  | Accessable x len => eqb_val V x || eqb_val V len
  | Map_element map_name key x size => eqb_val V key || eqb_val V x || eqb_val V size
  end;

Use_in_other_v t V :=
  match t with
  | Ebpf_map x id key value size => eqb_val V id || eqb_val V key || eqb_val V value || eqb_val V size 
  | Ebpf_context x data_start data_end l => eqb_val V data_start || eqb_val V data_end
  | Accessable x len => eqb_val V len
  | Map_element map_name key x size => eqb_val V x
  end;

Use_in_other_address t V :=
  match t with
  | Ebpf_map x _ _ _ _ => eqb_val V x
  | Ebpf_context x data_start data_end l => eqb_val V x 
  | Accessable x len => eqb_val V x
  | Map_element map_name key x size => eqb_val V x
  end;

max_ident_T t :=
  match t with
  | Ebpf_map x id key value size => Pos.max (max_ident_in_expr x) (Pos.max (Pos.max (max_ident_in_expr id) (max_ident_in_expr key)) (Pos.max (max_ident_in_expr value) (max_ident_in_expr size)))
  | Ebpf_context x data_start data_end l => Pos.max (max_ident_in_expr l) (Pos.max (max_ident_in_expr x) (Pos.max (max_ident_in_expr data_start) (max_ident_in_expr data_end)))
  | Accessable x len => Pos.max (max_ident_in_expr x) (max_ident_in_expr len)
  | Map_element map_name key x size => Pos.max (max_ident_in_expr key) (Pos.max (max_ident_in_expr x) (max_ident_in_expr size))
  end;

Changeval_other x y t :=
  match t with
  | Ebpf_map v id key value size => Ebpf_map (Changeval x y v) (Changeval x y id) (Changeval x y key) (Changeval x y value) (Changeval x y size)
  | Ebpf_context v data_start data_end l => Ebpf_context (Changeval x y v) (Changeval x y data_start) (Changeval x y data_end) (Changeval x y l)
  | Accessable v len => Accessable (Changeval x y v) (Changeval x y len)
  | Map_element map_name key v size => Map_element map_name (Changeval x y key) (Changeval x y v) (Changeval x y size)
  end;

is_emp t :=
  match t with
  | Ebpf_map _ _ _ _ size => eqb_val size (Ez_val 0)
  | Ebpf_context x data_start data_end l => eqb_val data_start data_end
  | Map_element map_name key x size => eqb_val size (Ez_val 0) || eqb_val x (Ez_val 0)
  | _ => false
  end; (* Here may be changed for Ebpf_context*)

Entailment_checker_with_EX Target Resource Exist_list :=
  match Target with
  | Ebpf_map x id key value size => 
     match Resource with
      | Other _ _ (Ebpf_map x' id' key' value' size') => 
        if (eqb_val x x') then 
        let Change_list := concat (map (fun a => match (fst a) with | V_vari i => if (Find Pos.eqb Exist_list i) then (i , snd a) :: nil else nil | _ => nil end) ((id , id') :: (key , key') :: (value , value') :: (size , size') :: nil)) in  
        mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list Change_list nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil) 
      else mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
      | _ => mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
     end  
  | Ebpf_context x data_start data_end l =>
    match Resource with
    | Other _ _ (Ebpf_context x1 data_start1 data_end1 l') =>
      if ((eqb_val x x1) && (eqb_val data_start data_start1) && (eqb_val data_end data_end1) && (eqb_val l l'))
      then mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list nil nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
      else mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
    | _ => mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
    end
  | Accessable x len => 
    match len with
      | V_vari i =>
      if (Find Pos.eqb Exist_list i) then
        match Resource with
          | Other _ _ (Accessable x1 len1) => if (eqb_val x x1)
              then mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list ((i , len1) :: nil) nil nil
              else mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
          | _ => mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
        end
      else match Resource with
             | Other _ _ (Accessable x1 len1) => if ((eqb_val x x1) && (eqb_val len len1))
                then mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list nil nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                else mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
             | _ => mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
           end
      | _ => match Resource with
              | Other _ _ (Accessable x1 len1) => if ((eqb_val x x1) && (eqb_val len len1))
                then mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list nil nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                else mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
              | _ => mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
             end
    end
  | Map_element map_name key x size =>
      match Resource with
        | Other _ _ (Map_element map_name' key' x1 size1) => 
            if ((eqb_val map_name map_name') && (eqb_val key key'))
            then match x with
                   | Vlist_vari i | V_vari i => if (Find Pos.eqb Exist_list i) 
                      then match size with
                             | V_vari i' => if (Find Pos.eqb Exist_list i') 
                                then 
                                mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list ((i, x1) :: (i' , size1) :: nil) nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                                else 
                                mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list ((i, x1) :: nil) nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                             | _ => 
                             mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list ((i, x1) :: nil) nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                           end
                      else match size with
                             | V_vari i' => if (Find Pos.eqb Exist_list i') 
                                then 
                                mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list ((i' , size1) :: nil) nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                                else 
                                mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list nil nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                             | _ => 
                             mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list nil nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                           end
                   | _ => match size with
                             | V_vari i' => if (Find Pos.eqb Exist_list i') 
                                then 
                                mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list ((i' , size1) :: nil) nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                                else 
                                mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list nil nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                             | _ => 
                             mk_S Ebpf_rule ebpf_type Ebpf_t nil nil Exist_list nil nil (Same_cancel Ebpf_rule ebpf_type Ebpf_t Resource :: nil)
                           end
                 end 
                else mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
        | _ => mk_S Ebpf_rule ebpf_type Ebpf_t ((Other _ _ Target) :: nil) (Resource :: nil) Exist_list nil nil nil
      end
  end;

(* The following methods are about processing of separations *)

Sep_simplify_prechecker sepT := None;

Sep_pure_fact_prechecker value type address := None;
  
Compare_sep check_expr sep :=
  match sep with
  | Data_at _ _ v t1 addr =>
    match addr with
    | Vfield_address addr' id' =>
      if (eqb_val addr' check_expr) then succ else fail
    | _ => fail
    end
  | _ => fail
  end;

Get_prop sepT check_expr := nil;

Get_simpl_expr_sep sepT := (nil,None);

Get_simpl_expr_data_at value type address := None;

Sep_split_prechecker sepT := None;

Sep_split_helper sepT propList max_ident := (Other _ _ sepT :: nil, nil);

}.

Definition Ebpf_Sep := Separation ebpf_type Ebpf_t.




