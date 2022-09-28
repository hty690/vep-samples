(* This file contain the function spec of Ebpf helper function *)

From VSTCForm Require Export VSTCFormlib.
From Ebpf Require Import Ebpfstatement Ebpfstate.

Local Open Scope option_monad_scope.
Local Open Scope val_scope.

Definition maxint := 4294967295%Z.

(** void *bpf_map_lookup_elem(struct bpf_map *map, const void *key) ------  1
    Description
      Perform a lookup in map for an entry associated to key.
    Return Map value associated to key, or NULL if no entry was found. *)

(** Maybe here is an unsafe behavior *)

Definition bpf_map_lookup_elem_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_map_lookup_elem_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: (Other _ _ (Map_element (V_vari 21%positive) (V_vari 22%positive) (Vlist_vari 20%positive) (V_vari 25%positive)) :: nil)) (20%positive :: nil).

(* Definition bpf_map_lookup_elem_post_1 := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Ez_val 0) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (nil). *)

Definition bpf_map_lookup_elem_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: nil) bpf_map_lookup_elem_pre  (mk_ret _ _ bpf_map_lookup_elem_post None).
    
(*Definition bpf_map_lookup_elem_spec_1 := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: nil) bpf_map_lookup_elem_pre  (mk_ret _ _ bpf_map_lookup_elem_post_1 None). *)

Definition bpf_map_lookup_elem_func_dec := (0%nat , (1%positive, bpf_map_lookup_elem_spec :: nil)).

(** long bpf_map_update_elem(struct bpf_map *map, const void *key, const void *value, u64 flags) ----  2
    Description
      Add or update the value of the entry associated to key in map with value. flags is one of:
      BPF_NOEXIST The entry for key must not exist in the map.
      BPF_EXIST The entry for key must already exist in the map.
      BPF_ANY No condition on the existence of the entry for key.
      Flag value BPF_NOEXIST cannot be used for maps of types BPF_MAP_TYPE_ARRAY or BPF_MAP_TYPE_PERCPU_ARRAY  (all elements always exist), the helper would return an error.
    Return 0 on success, or a negative error in case of failure. *)

Definition bpf_map_update_elem_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 28%positive) SCALAR_VALUE r4 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_map_update_elem_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 28%positive) SCALAR_VALUE r4 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).
      
Definition bpf_map_update_elem_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: 27%positive :: 28%positive :: nil) bpf_map_update_elem_pre  (mk_ret _ _ bpf_map_update_elem_post None).
    
Definition bpf_map_update_elem_func_dec := (0%nat , (2%positive, bpf_map_update_elem_spec :: nil)).

(** long bpf_map_delete_elem(struct bpf_map *map, const void *key) ------ 3
    Description
      Delete entry with key from map.
    Return 0 on success, or a negative error in case of failure. *)

(** Do not delete the map_element *)

Definition bpf_map_delete_elem_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: (Other _ _ (Map_element (V_vari 21%positive) (V_vari 22%positive) (Vlist_vari 27%positive) (V_vari 25%positive))) :: nil) (20%positive :: 27%positive :: nil).

Definition bpf_map_delete_elem_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Ez_val 0) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (nil).
      
Definition bpf_map_delete_elem_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: nil) bpf_map_delete_elem_pre  (mk_ret _ _ bpf_map_delete_elem_post None).

Definition bpf_map_delete_elem_func_dec := (0%nat , (3%positive, bpf_map_delete_elem_spec :: nil)).


(** long bpf_probe_read(void *dst, u32 size, const void *unsafe_ptr) -----  4
        Description
          For tracing programs, safely attempt to read size bytes from kernel space address unsafe_ptr and store the data in dst.
        Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_probe_read_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).

Definition bpf_probe_read_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).
      
Definition bpf_probe_read_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive :: nil) bpf_probe_read_pre  (mk_ret _ _ bpf_probe_read_post None).

Definition bpf_probe_read_func_dec := (0%nat , (4%positive, bpf_probe_read_spec :: nil)).

(** u64 bpf_ktime_get_ns(void) ------- 5
    Description
      Return the time elapsed since system boot, in nanoseconds.  Does not include time the system was suspended.  
    Return Current ktime. *)

Definition bpf_ktime_get_ns_pre := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).

Definition bpf_ktime_get_ns_post := mk_A _ _ (Be Pvle (Ez_val 0%Z) (V_vari 20%positive) :: nil) (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).
  
Definition bpf_ktime_get_ns_spec := mk_func _ _ nil nil bpf_ktime_get_ns_pre  (mk_ret _ _ bpf_ktime_get_ns_post None).

Definition bpf_ktime_get_ns_func_dec := (0%nat , (5%positive, bpf_ktime_get_ns_spec :: nil)).

(** long bpf_trace_printk(const char *fmt, u32 fmt_size, ...) ------  6
    Return The number of bytes written to the buffer, or a
                     negative error in case of failure.
*)

Definition bpf_trace_printk_pre := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).

Definition bpf_trace_printk_post := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).
  
Definition bpf_trace_printk_spec := mk_func _ _ nil nil bpf_trace_printk_pre  (mk_ret _ _ bpf_trace_printk_post None).

Definition bpf_trace_printk_func_dec := (0%nat , (6%positive, bpf_trace_printk_spec :: nil)).

(** u32 bpf_get_prandom_u32(void) -------  7
    Description
      Get a pseudo-random number.
    Return A random 32-bit unsigned value. *)

Definition bpf_get_prandom_u32_pre := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).

Definition bpf_get_prandom_u32_post := mk_A _ _ (Be Pvle (V_vari 20%positive) (Ez_val maxint) ::  Be Pvle (Ez_val 0%Z) (V_vari 20%positive) :: nil) (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).
      
Definition bpf_get_prandom_u32_spec := mk_func _ _ nil nil bpf_get_prandom_u32_pre  (mk_ret _ _ bpf_get_prandom_u32_post None).
    
Definition bpf_get_prandom_u32_func_dec := (0%nat , (7%positive, bpf_get_prandom_u32_spec :: nil)).

(** u32 bpf_get_smp_processor_id(void) -------  8
    Description
      Get the SMP (symmetric multiprocessing) processor id. Note that all programs run with preemption disabled, which means that the SMP processor id is stable during all the execution of the program.
    Return The SMP id of the processor running the program. *)

Definition bpf_get_smp_processor_id_pre := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).

Definition bpf_get_smp_processor_id_post := mk_A _ _ (Be Pvle (Ez_val 0%Z) (V_vari 20%positive) :: nil) (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).
  
Definition bpf_get_smp_processor_id_spec := mk_func _ _ nil nil bpf_get_smp_processor_id_pre  (mk_ret _ _ bpf_get_smp_processor_id_post None).

Definition bpf_get_smp_processor_id_func_dec := (0%nat , (8%positive, bpf_get_smp_processor_id_spec :: nil)).

(** long bpf_skb_store_bytes(struct sk_buff *skb, u32 offset, const
       void *from, u32 len, u64 flags) -----  9
      Description
        Store len bytes from address from into the packet associated to skb, at offset. flags are a combination of BPF_F_RECOMPUTE_CSUM (automatically recompute the checksum for the packet after storing the bytes) and BPF_F_INVALIDATE_HASH (set
        skb->hash, skb->swhash and skb->l4hash to 0).

        A call to this helper is susceptible to change the underlying packet buffer. Therefore, at load time, all checks on pointers previously done by the verifier are invalidated and must be performed again, if the helper is used in combination with direct packet access.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_skb_store_bytes_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).

Definition bpf_skb_store_bytes_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).
      
Definition bpf_skb_store_bytes_spec := mk_func _ _ nil nil bpf_skb_store_bytes_pre  (mk_ret _ _ bpf_skb_store_bytes_post None).
    
Definition bpf_skb_store_bytes_func_dec := (0%nat , (9%positive, bpf_skb_store_bytes_spec :: nil)).

(** long bpf_l3_csum_replace(struct sk_buff *skb, u32 offset, u64 from, u64 to, u64 size)    -------  10
       Description
          Recompute the layer 3 (e.g. IP) checksum for the packet associated to skb. Computation is incremental, so the helper must know the former value of the header field that was modified (from), the new value of this field (to), and the number of bytes (2 or 4) for this field, stored in size. Alternatively, it is possible to store the difference between the previous and the new values of the header field in to, by setting from and size to 0. For both methods, offset indicates the location of the IP checksum within the packet.

          This helper works in combination with bpf_csum_diff(), which does not update the checksum in-place, but offers more flexibility and can handle sizes larger than 2 or 4 for the checksum to update.

          A call to this helper is susceptible to change the underlying packet buffer. Therefore, at load time, all checks on pointers previously done by the verifier are invalidated and must be performed again, if the helper is used in combination with direct packet access.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_l3_csum_replace_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).

Definition bpf_l3_csum_replace_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).
      
Definition bpf_l3_csum_replace_spec := mk_func _ _ nil nil bpf_l3_csum_replace_pre  (mk_ret _ _ bpf_l3_csum_replace_post None).
    
Definition bpf_l3_csum_replace_func_dec := (0%nat , (10%positive, bpf_l3_csum_replace_spec :: nil)).


(** long bpf_l4_csum_replace(struct sk_buff *skb, u32 offset, u64 from, u64 to, u64 flags)    -------- 11
      Description
        Recompute the layer 4 (e.g. TCP, UDP, or ICMP) checksum for the packet associated to skb. Computation is incremental, so the helper must know the former value of the header field that was modified (from), the new value of this field (to), and the number of bytes (2 or 4) for this field, stored on the lowest four bits of flags. Alternatively, it is possible to store the difference between the previous and the new values of the header field in to, by setting from and the four lowest bits of flags to 0. For both methods, offset indicates the location of the IP checksum within the packet. In addition to the size of the field, flags can be added (bitwise OR) actual flags. With BPF_F_MARK_MANGLED_0, a null checksum is left untouched (unless BPF_F_MARK_ENFORCE is added as well), and for updates resulting in a null checksum the value is set to CSUM_MANGLED_0 instead. Flag BPF_F_PSEUDO_HDR indicates the checksum is to be computed against a pseudo-header.

        This helper works in combination with bpf_csum_diff(), which does not update the checksum in-place, but offers more flexibility and can handle sizes larger than 2 or 4 for the checksum to update.

        A call to this helper is susceptible to change the underlying packet buffer. Therefore, at load time, all checks on pointers previously done by the verifier are invalidated and must be performed again, if the helper is used in combination with direct packet access.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_l4_csum_replace_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).

Definition bpf_l4_csum_replace_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).
      
Definition bpf_l4_csum_replace_spec := mk_func _ _ nil nil bpf_l4_csum_replace_pre  (mk_ret _ _ bpf_l4_csum_replace_post None).
    
Definition bpf_l4_csum_replace_func_dec := (0%nat , (11%positive, bpf_l4_csum_replace_spec :: nil)).

(** long bpf_clone_redirect(struct sk_buff *skb, u32 ifindex, u64 flags) ----- 13
      Description
        Clone and redirect the packet associated to skb to another net device of index ifindex. Both ingress and egress interfaces can be used for redirection. The BPF_F_INGRESS value in flags is used to make the distinction (ingress path is selected if the flag is present, egress path otherwise).  This is the only flag supported for now.

        In comparison with bpf_redirect() helper, bpf_clone_redirect() has the associated cost of duplicating the packet buffer, but this can be executed out of the eBPF program. Conversely, bpf_redirect() is more efficient, but it is handled through an action code where the redirection happens only after the eBPF program has returned.

        A call to this helper is susceptible to change the underlying packet buffer. Therefore, at load time, all checks on pointers previously done by the verifier are invalidated and must be performed again, if the helper is used in combination with direct packet access.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_clone_redirect_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 ::  nil) (20%positive :: nil).

Definition bpf_clone_redirect_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: nil).

Definition bpf_clone_redirect_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: nil) bpf_clone_redirect_pre  (mk_ret _ _ bpf_clone_redirect_post None).
    
Definition bpf_clone_redirect_func_dec := (0%nat , (13%positive,bpf_clone_redirect_spec :: nil)).

(** u64 bpf_get_current_pid_tgid (void) -------  14
    Return A 64-bit integer containing the current tgid and pid, and created as such: current_task->tgid << 32 | current_task->pid. *)

Definition bpf_get_current_pid_tgid_pre := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).

Definition bpf_get_current_pid_tgid_post := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).
      
Definition bpf_get_current_pid_tgid_spec := mk_func _ _ nil nil bpf_get_current_pid_tgid_pre  (mk_ret _ _ bpf_get_current_pid_tgid_post None).
    
Definition bpf_get_current_pid_tgid_func_dec := (0%nat , (14%positive, bpf_get_current_pid_tgid_spec :: nil)).

(** u64 bpf_get_current_uid_gid (void) ------- 15
    Return A 64-bit integer containing the current GID and UID, and created as such: current_gid << 32 | current_uid. *)

Definition bpf_get_current_uid_gid_pre := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).

Definition bpf_get_current_uid_gid_post := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).
      
Definition bpf_get_current_uid_gid_spec := mk_func _ _ nil nil bpf_get_current_uid_gid_pre  (mk_ret _ _ bpf_get_current_uid_gid_post None).
    
Definition bpf_get_current_uid_gid_func_dec := (0%nat , (15%positive, bpf_get_current_uid_gid_spec :: nil)).

(** long bpf_get_current_comm(void *buf, u32 size_of_buf) --------- 16
              Description
                     Copy the comm attribute of the current task into
                     buf of size_of_buf. The comm attribute contains the
                     name of the executable (excluding the path) for the
                     current task. The size_of_buf must be strictly
                     positive. On success, the helper makes sure that
                     the buf is NUL-terminated. On failure, it is filled
                     with zeroes.

              Return 0 on success, or a negative error in case of
                     failure. *)
Definition bpf_get_current_comm_pre_1 := mk_A _ _ (Be Pvle (V_vari 22%positive) (V_vari 23%positive) :: nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Other ebpf_type Ebpf_t (Accessable (V_vari 21%positive) (V_vari 23%positive)) :: nil) (20%positive :: nil).

Definition bpf_get_current_comm_post_1 := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Ez_val 0) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Other ebpf_type Ebpf_t (Accessable (V_vari 21%positive) (V_vari 23%positive)) :: nil) nil.
      
Definition bpf_get_current_comm_spec_1 := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: nil) bpf_get_current_comm_pre_1  (mk_ret _ _ bpf_get_current_comm_post_1 None).


Definition bpf_get_current_comm_pre_2 := mk_A _ _ (In_bound (Ez_val (-512)) (Vbop Oadd (V_vari 21%positive) (V_vari 22%positive)) (Ez_val 0) :: nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: nil).

Definition bpf_get_current_comm_post_2 := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Ez_val 0) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) nil.
      
Definition bpf_get_current_comm_spec_2 := mk_func _ _ nil (21%positive :: 22%positive :: nil) bpf_get_current_comm_pre_2  (mk_ret _ _ bpf_get_current_comm_post_2 None).

Definition bpf_get_current_comm_func_dec := (0%nat , (16%positive, bpf_get_current_comm_spec_1 :: bpf_get_current_comm_spec_2 :: nil)).

(** long bpf_skb_vlan_push(struct sk_buff *skb, __be16 vlan_proto,
       u16 vlan_tci)   -------- 18
    Description
      Push a vlan_tci (VLAN tag control information) of protocol vlan_proto to the packet associated to skb, then update the checksum. Note that if vlan_proto is different from ETH_P_8021Q and ETH_P_8021AD, it is considered to be ETH_P_8021Q.

      A call to this helper is susceptible to change the underlying packet buffer. Therefore, at load time, all checks on pointers previously done by the verifier are invalidated and must be performed again, if the helper is used in combination with direct packet access.

    Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_skb_vlan_push_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 ::  nil) (20%positive :: nil).

Definition bpf_skb_vlan_push_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: nil).

Definition bpf_skb_vlan_push_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: nil) bpf_skb_vlan_push_pre  (mk_ret _ _ bpf_skb_vlan_push_post None).
    
Definition bpf_skb_vlan_push_func_dec := (0%nat , (18%positive,bpf_skb_vlan_push_spec :: nil)).

(** long bpf_skb_get_tunnel_key(struct sk_buff *skb, struct bpf_tunnel_key *key, u32 size, u64 flags)  ------- 20
      Description
        Get tunnel metadata. This helper takes a pointer key to an empty struct bpf_tunnel_key of size, that will be filled with tunnel metadata for the packet associated to skb.  The flags can be set to BPF_F_TUNINFO_IPV6, which indicates that the tunnel is based on IPv6 protocol instead of IPv4.

        The struct bpf_tunnel_key is an object that generalizes the principal parameters used by various tunneling protocols into a single struct. This way, it can be used to easily make a decision based on the contents of the encapsulation header,"summarized" in this struct. In particular, it holds the IP address of the remote end (IPv4 or IPv6, depending on the case) in key->remote_ipv4 or key->remote_ipv6. Also, this struct exposes the key->tunnel_id, which is generally mapped to a VNI (Virtual Network Identifier), making it programmable together with the bpf_skb_set_tunnel_key() helper.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_skb_get_tunnel_key_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) PTR_TO_CTX r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).

Definition bpf_skb_get_tunnel_key_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) PTR_TO_CTX r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).
      
Definition bpf_skb_get_tunnel_key_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive :: nil) bpf_skb_get_tunnel_key_pre  (mk_ret _ _ bpf_skb_get_tunnel_key_post None).

Definition bpf_skb_get_tunnel_key_func_dec := (0%nat , (20%positive, bpf_skb_get_tunnel_key_spec :: nil)).

(** long bpf_skb_set_tunnel_key(struct sk_buff *skb, struct
       bpf_tunnel_key *key, u32 size, u64 flags) ------ 21
      Description
        Populate tunnel metadata for packet associated to skb. The tunnel metadata is set to the contents of key, of size. The flags can be set to a combination of the following values:
          BPF_F_TUNINFO_IPV6
            Indicate that the tunnel is based on IPv6 protocol instead of IPv4.

          BPF_F_ZERO_CSUM_TX
            For IPv4 packets, add a flag to tunnel metadata indicating that checksum computation should be skipped and checksum set to zeroes.

          BPF_F_DONT_FRAGMENT
            Add a flag to tunnel metadata indicating that the packet should not be fragmented.

          BPF_F_SEQ_NUMBER
            Add a flag to tunnel metadata indicating that a sequence number should be added to tunnel header before sending the packet. This flag was added for GRE encapsulation, but might be used with other protocols as well in the future.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_skb_set_tunnel_key_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) PTR_TO_CTX r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).

Definition bpf_skb_set_tunnel_key_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) PTR_TO_CTX r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).
      
Definition bpf_skb_set_tunnel_key_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive :: nil) bpf_skb_set_tunnel_key_pre  (mk_ret _ _ bpf_skb_set_tunnel_key_post None).

Definition bpf_skb_set_tunnel_key_func_dec := (0%nat , (21%positive, bpf_skb_set_tunnel_key_spec :: nil)).


(** u64 bpf_perf_event_read(struct bpf_map *map, u64 flags)   ------ 22
      Description
        Read the value of a perf event counter. This helper relies on a map of type BPF_MAP_TYPE_PERF_EVENT_ARRAY. The nature of the perf event counter is selected when map is updated with perf event file descriptors. The map is an array whose size is the number of available CPUs, and each cell contains a value relative to one CPU. The value to retrieve is indicated by flags, that contains the index of the CPU to look up, masked with BPF_F_INDEX_MASK. Alternatively, flags can be set to BPF_F_CURRENT_CPU to indicate that the value for the current CPU should be retrieved.

        Note that before Linux 4.13, only hardware perf event can be retrieved.

      Return The value of the perf event counter read from the map, or a negative error code in case of failure.
*)


Definition bpf_perf_event_read_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_perf_event_read_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_perf_event_read_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: nil) bpf_perf_event_read_pre  (mk_ret _ _ bpf_perf_event_read_post None).

Definition bpf_perf_event_read_func_dec := (0%nat , (22%positive, bpf_perf_event_read_spec :: nil)).


(**  long bpf_redirect(u32 ifindex, u64 flags) ----- 23
       Description
         Redirect the packet to another net device of index ifindex.  This helper is somewhat similar to bpf_clone_redirect(), except that the packet is not cloned, which provides increased performance.

         Except for XDP, both ingress and egress interfaces can be used for redirection. The BPF_F_INGRESS value in flags is used to make the distinction (ingress path is selected if the flag is present, egress path otherwise). Currently, XDP only supports redirection to the egress interface, and accepts no flag at all.

        The same effect can also be attained with the more generic bpf_redirect_map(), which uses a BPF map to store the redirect target instead of providing it directly to the helper.

       Return For XDP, the helper returns XDP_REDIRECT on success or XDP_ABORTED on error. For other program types, the values are TC_ACT_REDIRECT on success or
      TC_ACT_SHOT on error.
*)

Definition bpf_redirect_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: nil).

Definition bpf_redirect_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: nil).

Definition bpf_redirect_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: nil) bpf_redirect_pre  (mk_ret _ _ bpf_redirect_post None).

Definition bpf_redirect_func_dec := (0%nat , (23%positive, bpf_redirect_spec :: nil)).

(** long bpf_perf_event_output(void *ctx, struct bpf_map *map, u64 flags, void *data, u64 size)   ----------  25
      Description
        Write raw data blob into a special BPF perf event held by map of type BPF_MAP_TYPE_PERF_EVENT_ARRAY. This perf event must have the following attributes: PERF_SAMPLE_RAW as sample_type, PERF_TYPE_SOFTWARE as type, and PERF_COUNT_SW_BPF_OUTPUT as config.

        The flags are used to indicate the index in map for which the value must be put, masked with BPF_F_INDEX_MASK.  Alternatively, flags can be set to BPF_F_CURRENT_CPU to indicate that the index of the current CPU core should be used.

        The value to write, of size, is passed through eBPF stack and pointed by data.

        The context of the program ctx needs also be passed to the helper.

        On user space, a program willing to read the values needs to call perf_event_open() on the perf event (either for one or for all CPUs) and to store the file descriptor into the map. This must be done before the eBPF program can send data into it. 
                  
      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_perf_event_output_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 22%positive) (V_vari 26%positive) (V_vari 27%positive) (V_vari 28%positive) (V_vari 29%positive))) :: nil) (20%positive :: nil).

Definition bpf_perf_event_output_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 22%positive) (V_vari 26%positive) (V_vari 27%positive) (V_vari 28%positive) (V_vari 29%positive))) :: nil) (20%positive :: nil).
      
Definition bpf_perf_event_output_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: 27%positive :: 28%positive :: 29%positive :: nil) bpf_perf_event_output_pre  (mk_ret _ _ bpf_perf_event_output_post None).
    
Definition bpf_perf_event_output_func_dec := (0%nat , (25%positive, bpf_perf_event_output_spec :: nil)).


(** long bpf_skb_load_bytes(const void *skb, u32 offset, void *to, u32 len)      --------- 26

              Description
                     This helper was provided as an easy way to load
                     data from a packet. It can be used to load len
                     bytes from offset from the packet associated to
                     skb, into the buffer pointed by to.

                     Since Linux 4.7, usage of this helper has mostly
                     been replaced by "direct packet access", enabling
                     packet data to be manipulated with skb->data and
                     skb->data_end pointing respectively to the first
                     byte of packet data and to the byte after the last
                     byte of packet data. However, it remains useful if
                     one wishes to read large quantities of data at once
                     from a packet into the eBPF stack.

              Return 0 on success, or a negative error in case of
                     failure.

*)

Definition bpf_skb_load_bytes_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) PTR_TO_CTX r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).

Definition bpf_skb_load_bytes_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) PTR_TO_CTX r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).
      
Definition bpf_skb_load_bytes_spec_1 := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive :: nil) bpf_skb_load_bytes_pre  (mk_ret _ _ bpf_skb_load_bytes_post None).

Definition bpf_skb_load_bytes_pre_2 := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).

Definition bpf_skb_load_bytes_post_2 := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).
      
Definition bpf_skb_load_bytes_spec_2 := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive :: nil) bpf_skb_load_bytes_pre_2  (mk_ret _ _ bpf_skb_load_bytes_post_2 None).

Definition bpf_skb_load_bytes_func_dec := (0%nat , (26%positive, bpf_skb_load_bytes_spec_1 :: bpf_skb_load_bytes_spec_2 :: nil)).

(* long bpf_get_stackid(void *ctx, struct bpf_map *map, u64 flags)    -------- 27

              Description
                     Walk a user or a kernel stack and return its id. To
                     achieve this, the helper needs ctx, which is a
                     pointer to the context on which the tracing program
                     is executed, and a pointer to a map of type
                     BPF_MAP_TYPE_STACK_TRACE.

                     The last argument, flags, holds the number of stack
                     frames to skip (from 0 to 255), masked with
                     BPF_F_SKIP_FIELD_MASK. The next bits can be used to
                     set a combination of the following flags:

                     BPF_F_USER_STACK
                            Collect a user space stack instead of a
                            kernel stack.

                     BPF_F_FAST_STACK_CMP
                            Compare stacks by hash only.

                     BPF_F_REUSE_STACKID
                            If two different stacks hash into the same
                            stackid, discard the old one.

                     The stack id retrieved is a 32 bit long integer
                     handle which can be further combined with other
                     data (including other stack ids) and used as a key
                     into maps. This can be useful for generating a
                     variety of graphs (such as flame graphs or off-cpu
                     graphs).

                     For walking a stack, this helper is an improvement
                     over bpf_probe_read(), which can be used with
                     unrolled loops but is not efficient and consumes a
                     lot of eBPF instructions.  Instead,
                     bpf_get_stackid() can collect up to
                     PERF_MAX_STACK_DEPTH both kernel and user frames.
                     Note that this limit can be controlled with the
                     sysctl program, and that it should be manually
                     increased in order to profile long user stacks
                     (such as stacks for Java programs). To do so, use:

                        # sysctl kernel.perf_event_max_stack=<new value>

              Return The positive or null stack id on success, or a
                     negative error in case of failure.
*)

Definition bpf_get_stackid_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 22%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_get_stackid_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 22%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_get_stackid_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: 27%positive :: nil) bpf_get_stackid_pre  (mk_ret _ _ bpf_get_stackid_post None).
    
Definition bpf_get_stackid_func_dec := (0%nat , (27%positive, bpf_get_stackid_spec :: nil)).

(** s64 bpf_csum_diff(__be32 *from, u32 from_size, __be32 *to, u32              --------- 28
       to_size, __wsum seed)

              Description
                     Compute a checksum difference, from the raw buffer
                     pointed by from, of length from_size (that must be
                     a multiple of 4), towards the raw buffer pointed by
                     to, of size to_size (same remark). An optional seed
                     can be added to the value (this can be cascaded,
                     the seed may come from a previous call to the
                     helper).

                     This is flexible enough to be used in several ways:

                     • With from_size == 0, to_size > 0 and seed set to
                       checksum, it can be used when pushing new data.

                     • With from_size > 0, to_size == 0 and seed set to
                       checksum, it can be used when removing data from
                       a packet.

                     • With from_size > 0, to_size > 0 and seed set to
                       0, it can be used to compute a diff. Note that
                       from_size and to_size do not need to be equal.

                     This helper can be used in combination with
                     bpf_l3_csum_replace() and bpf_l4_csum_replace(), to
                     which one can feed in the difference computed with
                     bpf_csum_diff().

              Return The checksum result, or a negative error code in
                     case of failure.




*)

Definition bpf_csum_diff_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).

Definition bpf_csum_diff_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).
      
Definition bpf_csum_diff_spec := mk_func _ _ nil nil bpf_csum_diff_pre  (mk_ret _ _ bpf_csum_diff_post None).
    
Definition bpf_csum_diff_func_dec := (0%nat , (28%positive, bpf_csum_diff_spec :: nil)).

(** long bpf_skb_change_proto(struct sk_buff *skb, __be16 proto, u64
       flags)             ---------- 31
    Description
      Change the protocol of the skb to proto. Currently supported are transition from IPv4 to IPv6, and from IPv6 to IPv4. The helper takes care of the groundwork for the transition, including resizing the socket buffer. The eBPF program is expected to fill the new headers, if any, via skb_store_bytes() and to recompute the checksums with bpf_l3_csum_replace() and bpf_l4_csum_replace(). The main case for this helper is to perform NAT64 operations out of an eBPF program.

      Internally, the GSO type is marked as dodgy so that headers are checked and segments are recalculated by the GSO/GRO engine.  The size for GSO target is adapted as well.

      All values for flags are reserved for future usage, and must be left at zero.

    Return 0 on success, or a negative error in case of failure.

*)

Definition bpf_skb_change_proto_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: nil).

Definition bpf_skb_change_proto_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: nil).

Definition bpf_skb_change_proto_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: nil) bpf_skb_change_proto_pre  (mk_ret _ _ bpf_skb_change_proto_post None).
    
Definition bpf_skb_change_proto_func_dec := (0%nat , (31%positive, bpf_skb_change_proto_spec :: nil)).

(** long bpf_skb_under_cgroup(struct sk_buff *skb, struct bpf_map
       *map, u32 index)       ------ 33
    Description
      Check whether skb is a descendant of the cgroup2 held by map of type BPF_MAP_TYPE_CGROUP_ARRAY, at index.
    
    Return The return value depends on the result of the test, and can be:
      • 0, if the skb failed the cgroup2 descendant test.
      • 1, if the skb succeeded the cgroup2 descendant test.
      • A negative error code, if an error occurred.

*)
Definition bpf_skb_under_cgroup_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 22%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_skb_under_cgroup_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 22%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_skb_under_cgroup_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: 27%positive :: nil) bpf_skb_under_cgroup_pre  (mk_ret _ _ bpf_skb_under_cgroup_post None).
    
Definition bpf_skb_under_cgroup_func_dec := (0%nat , (33%positive, bpf_skb_under_cgroup_spec :: nil)).

(** u32 bpf_get_hash_recalc(struct sk_buff *skb)    ------- 34
      Description
        Retrieve the hash of the packet, skb->hash. If it is not set, in particular if the hash was cleared due to mangling, recompute this hash. Later accesses to the hash can be done directly with skb->hash.

      Return The 32-bit hash.
*)

Definition bpf_get_hash_recalc_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: nil) (20%positive :: nil).

Definition bpf_get_hash_recalc_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: nil) (20%positive :: nil).
  
Definition bpf_get_hash_recalc_spec := mk_func _ _ nil (21%positive :: nil) bpf_get_hash_recalc_pre  (mk_ret _ _ bpf_get_hash_recalc_post None).

Definition bpf_get_hash_recalc_func_dec := (0%nat , (34%positive, bpf_get_hash_recalc_spec :: nil)).

(** u64 bpf_get_current_task(void)               -------- 35
      Return A pointer to the current task struct.
*)

Definition bpf_get_current_task_pre := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).

Definition bpf_get_current_task_post := mk_A _ _  nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).
  
Definition bpf_get_current_task_spec := mk_func _ _ nil nil bpf_get_current_task_pre  (mk_ret _ _ bpf_get_current_task_post None).

Definition bpf_get_current_task_func_dec := (0%nat , (35%positive, bpf_get_current_task_spec :: nil)).


(** long bpf_probe_write_user(void *dst, const void *src, u32 len)   ----- 36
      Description
        Attempt in a safe way to write len bytes from the buffer src to dst in memory. It only works for threads that are in user context, and dst must be a valid user space address.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_probe_write_user_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: nil).

Definition bpf_probe_write_user_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: nil).
      
Definition bpf_probe_write_user_spec := mk_func _ _ nil nil bpf_probe_write_user_pre  (mk_ret _ _ bpf_probe_write_user_post None).
    
Definition bpf_probe_write_user_func_dec := (0%nat , (56%positive, bpf_probe_write_user_spec :: nil)).

(** long bpf_current_task_under_cgroup(struct bpf_map *map, u32 index)   ------ 37
      Description
        Check whether the probe is being run is the context of a given subset of the cgroup2 hierarchy. The cgroup2 to test is held by map of type BPF_MAP_TYPE_CGROUP_ARRAY, at index.

      Return The return value depends on the result of the test, and can be:
        • 0, if the skb task belongs to the cgroup2.
        • 1, if the skb task does not belong to the cgroup2.
        • A negative error code, if an error occurred.
*)

Definition bpf_current_task_under_cgroup_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_current_task_under_cgroup_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Vlist_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_current_task_under_cgroup_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: nil) bpf_current_task_under_cgroup_pre  (mk_ret _ _ bpf_current_task_under_cgroup_post None).

Definition bpf_current_task_under_cgroup_func_dec := (0%nat , (37%positive, bpf_current_task_under_cgroup_spec :: nil)).

(** long bpf_skb_change_tail(struct sk_buff *skb, u32 len, u64 flags) ---- 38
      Description
        Resize (trim or grow) the packet associated to skb to the new len. The flags are reserved for future usage, and must be left at zero.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_skb_change_tail_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: nil).

Definition bpf_skb_change_tail_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: nil).
      
Definition bpf_skb_change_tail_spec := mk_func _ _ nil nil bpf_skb_change_tail_pre  (mk_ret _ _ bpf_skb_change_tail_post None).
    
Definition bpf_skb_change_tail_func_dec := (0%nat , (38%positive, bpf_skb_change_tail_spec :: nil)).


(** long bpf_skb_change_head(struct sk_buff *skb, u32 len, u64 flags)   ---- 43
      Description
        Grows headroom of packet associated to skb and adjusts the offset of the MAC header accordingly, adding len bytes of space. It automatically extends
        and reallocates memory as required.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_skb_change_head_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: nil).

Definition bpf_skb_change_head_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: nil).
      
Definition bpf_skb_change_head_spec := mk_func _ _ nil nil bpf_skb_change_head_pre  (mk_ret _ _ bpf_skb_change_head_post None).
    
Definition bpf_skb_change_head_func_dec := (0%nat , (43%positive, bpf_skb_change_head_spec :: nil)).

(** long bpf_xdp_adjust_head(struct xdp_buff *xdp_md, int delta)  ----- 44

              Description
                     Adjust (move) xdp_md->data by delta bytes. Note
                     that it is possible to use a negative value for
                     delta. This helper can be used to prepare the
                     packet for pushing or popping headers.

                     A call to this helper is susceptible to change the
                     underlying packet buffer. Therefore, at load time,
                     all checks on pointers previously done by the
                     verifier are invalidated and must be performed
                     again, if the helper is used in combination with
                     direct packet access.

              Return 0 on success, or a negative error in case of
                     failure.

*)

Definition bpf_xdp_adjust_head_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: 21%positive :: 22%positive :: nil).

Definition bpf_xdp_adjust_head_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: 21%positive :: 22%positive :: nil).

Definition bpf_xdp_adjust_head_spec := mk_func _ _ nil nil bpf_xdp_adjust_head_pre  (mk_ret _ _ bpf_xdp_adjust_head_post None).
    
Definition bpf_xdp_adjust_head_func_dec := (0%nat , (44%positive, bpf_xdp_adjust_head_spec :: nil)).

(** long bpf_probe_read_str(void *dst, u32 size, const void *unsafe_ptr) ---- 45
      Description
        Copy a NUL terminated string from an unsafe kernel address unsafe_ptr to dst. See
        bpf_probe_read_kernel_str() for more details.

      Return On success, the strictly positive length of the string, including the trailing NUL character. On error, a negative value.
*)

Definition bpf_probe_read_str_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: nil).

Definition bpf_probe_read_str_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: nil).
      
Definition bpf_probe_read_str_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: nil) bpf_probe_read_str_pre  (mk_ret _ _ bpf_probe_read_str_post None).

Definition bpf_probe_read_str_func_dec := (0%nat , (45%positive, bpf_probe_read_str_spec :: nil)).

(* long bpf_setsockopt(void *bpf_socket, int level, int optname,      ------- 49
       void *optval, int optlen)
     Description
       Emulate a call to setsockopt() on the socket associated to bpf_socket, which must be a full socket. The level at which the option resides and the name optname of the option must be specified, see setsockopt(2) for more information.  The option value of length optlen is pointed by optval.

       bpf_socket should be one of the following:
        • struct bpf_sock_ops for BPF_PROG_TYPE_SOCK_OPS.
        • struct bpf_sock_addr for BPF_CGROUP_INET4_CONNECT and BPF_CGROUP_INET6_CONNECT.

     Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_setsockopt_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).

Definition bpf_setsockopt_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).
      
Definition bpf_setsockopt_spec := mk_func _ _ nil nil bpf_setsockopt_pre  (mk_ret _ _ bpf_setsockopt_post None).
    
Definition bpf_setsockopt_func_dec := (0%nat , (49%positive, bpf_setsockopt_spec :: nil)).

(** long bpf_redirect_map(struct bpf_map *map, u32 key, u64 flags)  ----- 51
      Description
        Redirect the packet to the endpoint referenced by map at index key. Depending on its type, this map can contain references to net devices (for forwarding packets through other ports), or to CPUs (for redirecting XDP frames to another CPU; but this is only implemented for native XDP (with driver support) as of this writing).

        The lower two bits of flags are used as the return code if the map lookup fails. This is so that the return value can be one of the XDP program return codes up to XDP_TX, as chosen by the caller. Any higher bits in the flags argument must be unset.

      Return XDP_REDIRECT on success, or the value of the two lower bits of the flags argument on error.
*)

Definition bpf_redirect_map_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_redirect_map_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).
      
Definition bpf_redirect_map_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: 27%positive :: nil) bpf_redirect_map_pre  (mk_ret _ _ bpf_redirect_map_post None).
    
Definition bpf_redirect_map_func_dec := (0%nat , (51%positive, bpf_redirect_map_spec :: nil)).

(** long bpf_sock_map_update(struct bpf_sock_ops *skops, struct bpf_map *map, void *key, u64 flags)       ------- 53
    Description
      Add an entry to, or update a map referencing sockets. The skops is used as a new value for the entry associated to key. flags is one of:
      BPF_NOEXIST
        The entry for key must not exist in the map.
      BPF_EXIST
        The entry for key must already exist in the map.
      BPF_ANY
        No condition on the existence of the entry for key.
    
    Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_sock_map_update_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 22%positive) (V_vari 26%positive) (V_vari 27%positive) (V_vari 28%positive) (V_vari 29%positive))) :: nil) (20%positive :: nil).

Definition bpf_sock_map_update_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 22%positive) (V_vari 26%positive) (V_vari 27%positive) (V_vari 28%positive) (V_vari 29%positive))) :: nil) (20%positive :: nil).
      
Definition bpf_sock_map_update_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive :: 26%positive :: 27%positive :: 28%positive :: 29%positive :: nil) bpf_sock_map_update_pre  (mk_ret _ _ bpf_sock_map_update_post None).
    
Definition bpf_sock_map_update_func_dec := (0%nat , (53%positive, bpf_sock_map_update_spec :: nil)).

(** long bpf_xdp_adjust_meta(struct xdp_buff *xdp_md, int delta)    ---- 54
    Description
      Adjust the address pointed by xdp_md->data_meta by delta (which can be positive or negative). Note that this operation modifies the address stored in xdp_md->data, so the latter must be loaded only after the helper has been called.

    Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_xdp_adjust_meta_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: 21%positive :: 22%positive :: nil).

Definition bpf_xdp_adjust_meta_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: 21%positive :: 22%positive :: nil).

Definition bpf_xdp_adjust_meta_spec := mk_func _ _ nil nil bpf_xdp_adjust_head_pre  (mk_ret _ _ bpf_xdp_adjust_head_post None).
    
Definition bpf_xdp_adjust_meta_func_dec := (0%nat , (54%positive, bpf_xdp_adjust_meta_spec :: nil)).

(** long bpf_perf_event_read_value(struct bpf_map *map, u64 flags, struct bpf_perf_event_value *buf, u32 buf_size)    ------- 55 
    Description
      Read the value of a perf event counter, and store it into buf of size buf_size. This helper relies on a map of type BPF_MAP_TYPE_PERF_EVENT_ARRAY. The nature of the perf event counter is selected when map is updated with perf event file descriptors. The map is an array whose size is the number of available CPUs, and each cell contains a value relative to one CPU. The value to retrieve is indicated by flags, that contains the index of the CPU to look up, masked with BPF_F_INDEX_MASK. Alternatively, flags can be set to BPF_F_CURRENT_CPU to indicate that the value for the current CPU should be retrieved.

    Return 0 on success, or a negative error in case of failure.

*)

Definition bpf_perf_event_read_value_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 28%positive) SCALAR_VALUE r4 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_perf_event_read_value_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 28%positive) SCALAR_VALUE r4 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).
      
Definition bpf_perf_event_read_value_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: 27%positive :: 28%positive :: nil) bpf_perf_event_read_value_pre  (mk_ret _ _ bpf_perf_event_read_value_post None).
    
Definition bpf_perf_event_read_value_func_dec := (0%nat , (55%positive, bpf_perf_event_read_value_spec :: nil)).

(** long bpf_perf_prog_read_value(struct bpf_perf_event_data *ctx,
       struct bpf_perf_event_value *buf, u32 buf_size)                   ------- 56

              Description
                     For en eBPF program attached to a perf event,
                     retrieve the value of the event counter associated
                     to ctx and store it in the structure pointed by buf
                     and of size buf_size. Enabled and running times are
                     also stored in the structure (see description of
                     helper bpf_perf_event_read_value() for more
                     details).

              Return 0 on success, or a negative error in case of
                     failure.
*)

Definition bpf_perf_prog_read_value_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: nil).

Definition bpf_perf_prog_read_value_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: nil).
      
Definition bpf_perf_prog_read_value_spec := mk_func _ _ nil nil bpf_perf_prog_read_value_pre  (mk_ret _ _ bpf_perf_prog_read_value_post None).
    
Definition bpf_perf_prog_read_value_func_dec := (0%nat , (56%positive, bpf_perf_prog_read_value_spec :: nil)).

(** long bpf_getsockopt(void *bpf_socket, int level, int optname,        -------- 57
       void *optval, int optlen)

              Description
                     Emulate a call to getsockopt() on the socket
                     associated to bpf_socket, which must be a full
                     socket. The level at which the option resides and
                     the name optname of the option must be specified,
                     see getsockopt(2) for more information.  The
                     retrieved value is stored in the structure pointed
                     by opval and of length optlen.

                     bpf_socket should be one of the following:

                     • struct bpf_sock_ops for BPF_PROG_TYPE_SOCK_OPS.

                     • struct bpf_sock_addr for BPF_CGROUP_INET4_CONNECT
                       and BPF_CGROUP_INET6_CONNECT.

                     This helper actually implements a subset of
                     getsockopt().  It supports the following levels:

                     • IPPROTO_TCP, which supports optname
                       TCP_CONGESTION.

                     • IPPROTO_IP, which supports optname IP_TOS.

                     • IPPROTO_IPV6, which supports optname IPV6_TCLASS.

              Return 0 on success, or a negative error in case of
                     failure.

*)

Definition bpf_getsockopt_pre := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).

Definition bpf_getsockopt_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: Temp R5 r5 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: Data_at _ _ (V_vari 25%positive) SCALAR_VALUE r5 :: nil) (20%positive :: 21%positive :: 22%positive :: 23%positive :: 24%positive :: 25%positive :: nil).
      
Definition bpf_getsockopt_spec := mk_func _ _ nil nil bpf_getsockopt_pre  (mk_ret _ _ bpf_getsockopt_post None).
    
Definition bpf_getsockopt_func_dec := (0%nat , (57%positive, bpf_getsockopt_spec :: nil)).

(** long bpf_override_return(struct pt_regs *regs, u64 rc)   ------ 58
    Description
      Used for error injection, this helper uses kprobes to override the return value of the probed function, and to set it to rc.  The first argument is the context regs on which the kprobe works.
    Return 0
*)

Definition bpf_override_return_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: nil).

Definition bpf_override_return_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (Ez_val 0) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (nil).

Definition bpf_override_return_spec := mk_func _ _ nil (21%positive :: 22%positive :: nil) bpf_override_return_pre  (mk_ret _ _ bpf_override_return_post None).
    
Definition bpf_override_return_func_dec := (0%nat , (58%positive, bpf_override_return_spec :: nil)).

(** long bpf_xdp_adjust_tail(struct xdp_buff *xdp_md, int delta)      ------- 65

              Description
                     Adjust (move) xdp_md->data_end by delta bytes. It
                     is possible to both shrink and grow the packet
                     tail.  Shrink done via delta being a negative
                     integer.

                     A call to this helper is susceptible to change the
                     underlying packet buffer. Therefore, at load time,
                     all checks on pointers previously done by the
                     verifier are invalidated and must be performed
                     again, if the helper is used in combination with
                     direct packet access.

              Return 0 on success, or a negative error in case of
                     failure.
*)

Definition bpf_xdp_adjust_tail_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: 21%positive :: 22%positive :: nil).

Definition bpf_xdp_adjust_tail_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: nil) (20%positive :: 21%positive :: 22%positive :: nil).

Definition bpf_xdp_adjust_tail_spec := mk_func _ _ nil nil bpf_xdp_adjust_tail_pre  (mk_ret _ _ bpf_xdp_adjust_tail_post None).
    
Definition bpf_xdp_adjust_tail_func_dec := (0%nat , (65%positive, bpf_xdp_adjust_tail_spec :: nil)).


(** long bpf_fib_lookup(void *ctx, struct bpf_fib_lookup *params, int
       plen, u32 flags)           --------- 69
    Description
      Do FIB lookup in kernel tables using parameters in params.  If lookup is successful and result shows packet is to be forwarded, the neighbor tables are searched for the nexthop.  If successful (ie., FIB lookup shows forwarding and nexthop is resolved), the nexthop address is returned in ipv4_dst or ipv6_dst based on family, smac is set to mac address of egress device, dmac is set to nexthop mac address, rt_metric is set to metric from route (IPv4/IPv6 only), and ifindex is set to the device index of the nexthop from the FIB lookup.

      plen argument is the size of the passed in struct.
      flags argument can be a combination of one or more of the following values:

      BPF_FIB_LOOKUP_DIRECT
        Do a direct table lookup vs full lookup using FIB rules.

      BPF_FIB_LOOKUP_OUTPUT
        Perform lookup from an egress perspective (default is ingress).

      ctx is either struct xdp_md for XDP programs or struct sk_buff tc cls_act programs.

    Return
      • < 0 if any input argument is invalid
      • 0 on success (packet is forwarded, nexthop neighbor exists)
      • > 0 one of BPF_FIB_LKUP_RET_ codes explaining why the packet is not forwarded or needs assist from full stack
*)

Definition bpf_fib_lookup_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).

Definition bpf_fib_lookup_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).
      
Definition bpf_fib_lookup_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive :: nil) bpf_fib_lookup_pre  (mk_ret _ _ bpf_fib_lookup_post None).
    
Definition bpf_fib_lookup_func_dec := (0%nat , (69%positive, bpf_fib_lookup_spec :: nil)).

(** long bpf_map_push_elem(struct bpf_map *map, const void *value, u64 flags) ---- 87
    Description
      Push an element value in map. flags is one of:
      BPF_EXIST : If the queue/stack is full, the oldest element is removed to make room for this.
      Return 0 on success, or a negative error in case of failure. *)

Definition bpf_map_push_elem_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_map_push_elem_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 27%positive) SCALAR_VALUE r3 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).
      
Definition bpf_map_push_elem_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: 27%positive :: nil) bpf_map_push_elem_pre  (mk_ret _ _ bpf_map_push_elem_post None).
    
Definition bpf_map_push_elem_func_dec := (0%nat , (87%positive, bpf_map_push_elem_spec :: nil)).

(** long bpf_map_pop_elem(struct bpf_map *map, void *value)   ----- 88 
    Description
      Pop an element from map.
    Return 0 on success, or a negative error in case of failure. *)

Definition bpf_map_pop_elem_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_map_pop_elem_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).
      
Definition bpf_map_pop_elem_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: nil) bpf_map_pop_elem_pre  (mk_ret _ _ bpf_map_pop_elem_post None).
    
Definition bpf_map_pop_elem_func_dec := (0%nat , (88%positive, bpf_map_pop_elem_spec :: nil)).

(** long bpf_map_peek_elem(struct bpf_map *map, void *value) ------ 89
    Description
      Get an element from map without removing it.
    Return 0 on success, or a negative error in case of failure. *)

Definition bpf_map_peek_elem_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).

Definition bpf_map_peek_elem_post := mk_A _ _ nil (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 ::Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: (Other ebpf_type Ebpf_t (Ebpf_map (V_vari 21%positive) (V_vari 23%positive) (V_vari 24%positive) (V_vari 25%positive) (V_vari 26%positive))) :: nil) (20%positive :: nil).
      
Definition bpf_map_peek_elem_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive:: 25%positive :: 26%positive :: nil) bpf_map_peek_elem_pre  (mk_ret _ _ bpf_map_peek_elem_post None).
    
Definition bpf_map_peek_elem_func_dec := (0%nat , (89%positive, bpf_map_peek_elem_spec :: nil)).

(**long bpf_probe_read_kernel(void *dst, u32 size, const void *unsafe_ptr)  ------  113
     Description
       Safely attempt to read size bytes from kernel space address unsafe_ptr and store the data in dst.

     Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_probe_read_kernel_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: nil).

Definition bpf_probe_read_kernel_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: nil) (20%positive :: nil).
      
Definition bpf_probe_read_kernel_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: nil) bpf_probe_read_kernel_pre  (mk_ret _ _ bpf_probe_read_kernel_post None).

Definition bpf_probe_read_kernel_func_dec := (0%nat , (113%positive, bpf_probe_read_kernel_spec :: nil)).

(** u64 bpf_jiffies64(void)             ------ 118
    Description
      Obtain the 64bit jiffies
    Return The 64 bit jiffies
*)

Definition bpf_jiffies64_pre := mk_A _ _ nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).

Definition bpf_jiffies64_post := mk_A _ _  nil (Temp R0 r0 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: nil) (20%positive :: nil).
  
Definition bpf_jiffies64_spec := mk_func _ _ nil nil bpf_jiffies64_pre  (mk_ret _ _ bpf_jiffies64_post None).

Definition bpf_jiffies64_func_dec := (0%nat , (118%positive, bpf_jiffies64_spec :: nil)).

(** long bpf_ringbuf_output(void *ringbuf, void *data, u64 size, u64
       flags)     ------ 130
      Description
        Copy size bytes from data into a ring buffer ringbuf.  If BPF_RB_NO_WAKEUP is specified in flags, no notification of new data availability is sent.  If BPF_RB_FORCE_WAKEUP is specified in flags, notification of new data availability is sent unconditionally.

      Return 0 on success, or a negative error in case of failure.
*)

Definition bpf_ringbuf_output_pre := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).

Definition bpf_ringbuf_output_post := mk_A _ _ (nil) (Temp R0 r0 :: Temp R1 r1 :: Temp R2 r2 :: Temp R3 r3 :: Temp R4 r4 :: nil) (Data_at _ _ (V_vari 20%positive) SCALAR_VALUE r0 :: Data_at _ _ (V_vari 21%positive) SCALAR_VALUE r1 :: Data_at _ _ (V_vari 22%positive) SCALAR_VALUE r2 :: Data_at _ _ (V_vari 23%positive) SCALAR_VALUE r3 :: Data_at _ _ (V_vari 24%positive) SCALAR_VALUE r4 :: nil) (20%positive :: nil).
      
Definition bpf_ringbuf_output_spec := mk_func _ _ nil (21%positive :: 22%positive :: 23%positive :: 24%positive :: nil) bpf_ringbuf_output_pre  (mk_ret _ _ bpf_ringbuf_output_post None).
    
Definition bpf_ringbuf_output_func_dec := (0%nat , (130%positive, bpf_ringbuf_output_spec :: nil)).

Definition Helper_func : list (nat * funcspec) :=  
  bpf_map_lookup_elem_func_dec :: bpf_map_update_elem_func_dec :: bpf_map_delete_elem_func_dec :: bpf_probe_read_func_dec :: bpf_ktime_get_ns_func_dec :: bpf_trace_printk_func_dec ::
  bpf_get_prandom_u32_func_dec :: bpf_get_smp_processor_id_func_dec ::
  bpf_skb_store_bytes_func_dec :: bpf_l3_csum_replace_func_dec :: 
  bpf_l4_csum_replace_func_dec :: bpf_clone_redirect_func_dec ::
  bpf_get_current_pid_tgid_func_dec :: bpf_get_current_uid_gid_func_dec ::
  bpf_get_current_comm_func_dec :: bpf_skb_vlan_push_func_dec :: 
  bpf_skb_get_tunnel_key_func_dec :: bpf_skb_set_tunnel_key_func_dec :: 
  bpf_perf_event_read_func_dec :: bpf_redirect_func_dec :: 
  bpf_perf_event_output_func_dec :: bpf_skb_load_bytes_func_dec :: 
  bpf_get_stackid_func_dec :: bpf_csum_diff_func_dec ::
  bpf_skb_change_proto_func_dec :: bpf_skb_under_cgroup_func_dec ::
  bpf_get_hash_recalc_func_dec :: bpf_get_current_task_func_dec ::
  bpf_probe_write_user_func_dec :: bpf_current_task_under_cgroup_func_dec ::
  bpf_skb_change_tail_func_dec :: bpf_skb_change_head_func_dec :: 
  bpf_xdp_adjust_head_func_dec :: bpf_probe_read_str_func_dec :: 
  bpf_setsockopt_func_dec :: bpf_redirect_map_func_dec :: 
  bpf_sock_map_update_func_dec :: bpf_xdp_adjust_meta_func_dec :: 
  bpf_perf_event_read_value_func_dec :: bpf_perf_prog_read_value_func_dec ::
  bpf_getsockopt_func_dec :: bpf_override_return_func_dec ::
  bpf_xdp_adjust_tail_func_dec :: bpf_fib_lookup_func_dec :: 
  bpf_map_push_elem_func_dec :: bpf_map_pop_elem_func_dec :: bpf_map_peek_elem_func_dec :: bpf_probe_read_kernel_func_dec :: bpf_jiffies64_func_dec :: bpf_ringbuf_output_func_dec :: 
  nil.