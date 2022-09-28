open Assdef
open BinNums
open Datatypes
open Ebpfstate
open Funcspec
open Localdef
open Map_sep
open Propdef
open Sepdef
open Exprdef

val maxint : coq_Z

val bpf_map_lookup_elem_pre : ebpf_type coq_Prod_assert

val bpf_map_lookup_elem_post : ebpf_type coq_Prod_assert

val bpf_map_lookup_elem_spec : ebpf_type funcdes

val bpf_map_lookup_elem_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_map_update_elem_pre : ebpf_type coq_Prod_assert

val bpf_map_update_elem_post : ebpf_type coq_Prod_assert

val bpf_map_update_elem_spec : ebpf_type funcdes

val bpf_map_update_elem_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_map_delete_elem_pre : ebpf_type coq_Prod_assert

val bpf_map_delete_elem_post : ebpf_type coq_Prod_assert

val bpf_map_delete_elem_spec : ebpf_type funcdes

val bpf_map_delete_elem_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_probe_read_pre : ebpf_type coq_Prod_assert

val bpf_probe_read_post : ebpf_type coq_Prod_assert

val bpf_probe_read_spec : ebpf_type funcdes

val bpf_probe_read_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_ktime_get_ns_pre : ebpf_type coq_Prod_assert

val bpf_ktime_get_ns_post : ebpf_type coq_Prod_assert

val bpf_ktime_get_ns_spec : ebpf_type funcdes

val bpf_ktime_get_ns_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_trace_printk_pre : ebpf_type coq_Prod_assert

val bpf_trace_printk_post : ebpf_type coq_Prod_assert

val bpf_trace_printk_spec : ebpf_type funcdes

val bpf_trace_printk_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_get_prandom_u32_pre : ebpf_type coq_Prod_assert

val bpf_get_prandom_u32_post : ebpf_type coq_Prod_assert

val bpf_get_prandom_u32_spec : ebpf_type funcdes

val bpf_get_prandom_u32_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_get_smp_processor_id_pre : ebpf_type coq_Prod_assert

val bpf_get_smp_processor_id_post : ebpf_type coq_Prod_assert

val bpf_get_smp_processor_id_spec : ebpf_type funcdes

val bpf_get_smp_processor_id_func_dec :
  nat * (positive * ebpf_type funcdes list)

val bpf_skb_store_bytes_pre : ebpf_type coq_Prod_assert

val bpf_skb_store_bytes_post : ebpf_type coq_Prod_assert

val bpf_skb_store_bytes_spec : ebpf_type funcdes

val bpf_skb_store_bytes_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_l3_csum_replace_pre : ebpf_type coq_Prod_assert

val bpf_l3_csum_replace_post : ebpf_type coq_Prod_assert

val bpf_l3_csum_replace_spec : ebpf_type funcdes

val bpf_l3_csum_replace_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_l4_csum_replace_pre : ebpf_type coq_Prod_assert

val bpf_l4_csum_replace_post : ebpf_type coq_Prod_assert

val bpf_l4_csum_replace_spec : ebpf_type funcdes

val bpf_l4_csum_replace_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_clone_redirect_pre : ebpf_type coq_Prod_assert

val bpf_clone_redirect_post : ebpf_type coq_Prod_assert

val bpf_clone_redirect_spec : ebpf_type funcdes

val bpf_clone_redirect_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_get_current_pid_tgid_pre : ebpf_type coq_Prod_assert

val bpf_get_current_pid_tgid_post : ebpf_type coq_Prod_assert

val bpf_get_current_pid_tgid_spec : ebpf_type funcdes

val bpf_get_current_pid_tgid_func_dec :
  nat * (positive * ebpf_type funcdes list)

val bpf_get_current_uid_gid_pre : ebpf_type coq_Prod_assert

val bpf_get_current_uid_gid_post : ebpf_type coq_Prod_assert

val bpf_get_current_uid_gid_spec : ebpf_type funcdes

val bpf_get_current_uid_gid_func_dec :
  nat * (positive * ebpf_type funcdes list)

val bpf_get_current_comm_pre_1 : ebpf_type coq_Prod_assert

val bpf_get_current_comm_post_1 : ebpf_type coq_Prod_assert

val bpf_get_current_comm_spec_1 : ebpf_type funcdes

val bpf_get_current_comm_pre_2 : ebpf_type coq_Prod_assert

val bpf_get_current_comm_post_2 : ebpf_type coq_Prod_assert

val bpf_get_current_comm_spec_2 : ebpf_type funcdes

val bpf_get_current_comm_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_skb_vlan_push_pre : ebpf_type coq_Prod_assert

val bpf_skb_vlan_push_post : ebpf_type coq_Prod_assert

val bpf_skb_vlan_push_spec : ebpf_type funcdes

val bpf_skb_vlan_push_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_skb_get_tunnel_key_pre : ebpf_type coq_Prod_assert

val bpf_skb_get_tunnel_key_post : ebpf_type coq_Prod_assert

val bpf_skb_get_tunnel_key_spec : ebpf_type funcdes

val bpf_skb_get_tunnel_key_func_dec :
  nat * (positive * ebpf_type funcdes list)

val bpf_skb_set_tunnel_key_pre : ebpf_type coq_Prod_assert

val bpf_skb_set_tunnel_key_post : ebpf_type coq_Prod_assert

val bpf_skb_set_tunnel_key_spec : ebpf_type funcdes

val bpf_skb_set_tunnel_key_func_dec :
  nat * (positive * ebpf_type funcdes list)

val bpf_perf_event_read_pre : ebpf_type coq_Prod_assert

val bpf_perf_event_read_post : ebpf_type coq_Prod_assert

val bpf_perf_event_read_spec : ebpf_type funcdes

val bpf_perf_event_read_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_redirect_pre : ebpf_type coq_Prod_assert

val bpf_redirect_post : ebpf_type coq_Prod_assert

val bpf_redirect_spec : ebpf_type funcdes

val bpf_redirect_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_perf_event_output_pre : ebpf_type coq_Prod_assert

val bpf_perf_event_output_post : ebpf_type coq_Prod_assert

val bpf_perf_event_output_spec : ebpf_type funcdes

val bpf_perf_event_output_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_skb_load_bytes_pre : ebpf_type coq_Prod_assert

val bpf_skb_load_bytes_post : ebpf_type coq_Prod_assert

val bpf_skb_load_bytes_spec_1 : ebpf_type funcdes

val bpf_skb_load_bytes_pre_2 : ebpf_type coq_Prod_assert

val bpf_skb_load_bytes_post_2 : ebpf_type coq_Prod_assert

val bpf_skb_load_bytes_spec_2 : ebpf_type funcdes

val bpf_skb_load_bytes_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_get_stackid_pre : ebpf_type coq_Prod_assert

val bpf_get_stackid_post : ebpf_type coq_Prod_assert

val bpf_get_stackid_spec : ebpf_type funcdes

val bpf_get_stackid_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_csum_diff_pre : ebpf_type coq_Prod_assert

val bpf_csum_diff_post : ebpf_type coq_Prod_assert

val bpf_csum_diff_spec : ebpf_type funcdes

val bpf_csum_diff_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_skb_change_proto_pre : ebpf_type coq_Prod_assert

val bpf_skb_change_proto_post : ebpf_type coq_Prod_assert

val bpf_skb_change_proto_spec : ebpf_type funcdes

val bpf_skb_change_proto_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_skb_under_cgroup_pre : ebpf_type coq_Prod_assert

val bpf_skb_under_cgroup_post : ebpf_type coq_Prod_assert

val bpf_skb_under_cgroup_spec : ebpf_type funcdes

val bpf_skb_under_cgroup_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_get_hash_recalc_pre : ebpf_type coq_Prod_assert

val bpf_get_hash_recalc_post : ebpf_type coq_Prod_assert

val bpf_get_hash_recalc_spec : ebpf_type funcdes

val bpf_get_hash_recalc_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_get_current_task_pre : ebpf_type coq_Prod_assert

val bpf_get_current_task_post : ebpf_type coq_Prod_assert

val bpf_get_current_task_spec : ebpf_type funcdes

val bpf_get_current_task_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_probe_write_user_pre : ebpf_type coq_Prod_assert

val bpf_probe_write_user_post : ebpf_type coq_Prod_assert

val bpf_probe_write_user_spec : ebpf_type funcdes

val bpf_probe_write_user_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_current_task_under_cgroup_pre : ebpf_type coq_Prod_assert

val bpf_current_task_under_cgroup_post : ebpf_type coq_Prod_assert

val bpf_current_task_under_cgroup_spec : ebpf_type funcdes

val bpf_current_task_under_cgroup_func_dec :
  nat * (positive * ebpf_type funcdes list)

val bpf_skb_change_tail_pre : ebpf_type coq_Prod_assert

val bpf_skb_change_tail_post : ebpf_type coq_Prod_assert

val bpf_skb_change_tail_spec : ebpf_type funcdes

val bpf_skb_change_tail_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_skb_change_head_pre : ebpf_type coq_Prod_assert

val bpf_skb_change_head_post : ebpf_type coq_Prod_assert

val bpf_skb_change_head_spec : ebpf_type funcdes

val bpf_skb_change_head_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_xdp_adjust_head_pre : ebpf_type coq_Prod_assert

val bpf_xdp_adjust_head_post : ebpf_type coq_Prod_assert

val bpf_xdp_adjust_head_spec : ebpf_type funcdes

val bpf_xdp_adjust_head_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_probe_read_str_pre : ebpf_type coq_Prod_assert

val bpf_probe_read_str_post : ebpf_type coq_Prod_assert

val bpf_probe_read_str_spec : ebpf_type funcdes

val bpf_probe_read_str_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_setsockopt_pre : ebpf_type coq_Prod_assert

val bpf_setsockopt_post : ebpf_type coq_Prod_assert

val bpf_setsockopt_spec : ebpf_type funcdes

val bpf_setsockopt_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_redirect_map_pre : ebpf_type coq_Prod_assert

val bpf_redirect_map_post : ebpf_type coq_Prod_assert

val bpf_redirect_map_spec : ebpf_type funcdes

val bpf_redirect_map_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_sock_map_update_pre : ebpf_type coq_Prod_assert

val bpf_sock_map_update_post : ebpf_type coq_Prod_assert

val bpf_sock_map_update_spec : ebpf_type funcdes

val bpf_sock_map_update_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_xdp_adjust_meta_spec : ebpf_type funcdes

val bpf_xdp_adjust_meta_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_perf_event_read_value_pre : ebpf_type coq_Prod_assert

val bpf_perf_event_read_value_post : ebpf_type coq_Prod_assert

val bpf_perf_event_read_value_spec : ebpf_type funcdes

val bpf_perf_event_read_value_func_dec :
  nat * (positive * ebpf_type funcdes list)

val bpf_perf_prog_read_value_pre : ebpf_type coq_Prod_assert

val bpf_perf_prog_read_value_post : ebpf_type coq_Prod_assert

val bpf_perf_prog_read_value_spec : ebpf_type funcdes

val bpf_perf_prog_read_value_func_dec :
  nat * (positive * ebpf_type funcdes list)

val bpf_getsockopt_pre : ebpf_type coq_Prod_assert

val bpf_getsockopt_post : ebpf_type coq_Prod_assert

val bpf_getsockopt_spec : ebpf_type funcdes

val bpf_getsockopt_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_override_return_pre : ebpf_type coq_Prod_assert

val bpf_override_return_post : ebpf_type coq_Prod_assert

val bpf_override_return_spec : ebpf_type funcdes

val bpf_override_return_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_xdp_adjust_tail_pre : ebpf_type coq_Prod_assert

val bpf_xdp_adjust_tail_post : ebpf_type coq_Prod_assert

val bpf_xdp_adjust_tail_spec : ebpf_type funcdes

val bpf_xdp_adjust_tail_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_fib_lookup_pre : ebpf_type coq_Prod_assert

val bpf_fib_lookup_post : ebpf_type coq_Prod_assert

val bpf_fib_lookup_spec : ebpf_type funcdes

val bpf_fib_lookup_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_map_push_elem_pre : ebpf_type coq_Prod_assert

val bpf_map_push_elem_post : ebpf_type coq_Prod_assert

val bpf_map_push_elem_spec : ebpf_type funcdes

val bpf_map_push_elem_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_map_pop_elem_pre : ebpf_type coq_Prod_assert

val bpf_map_pop_elem_post : ebpf_type coq_Prod_assert

val bpf_map_pop_elem_spec : ebpf_type funcdes

val bpf_map_pop_elem_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_map_peek_elem_pre : ebpf_type coq_Prod_assert

val bpf_map_peek_elem_post : ebpf_type coq_Prod_assert

val bpf_map_peek_elem_spec : ebpf_type funcdes

val bpf_map_peek_elem_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_probe_read_kernel_pre : ebpf_type coq_Prod_assert

val bpf_probe_read_kernel_post : ebpf_type coq_Prod_assert

val bpf_probe_read_kernel_spec : ebpf_type funcdes

val bpf_probe_read_kernel_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_jiffies64_pre : ebpf_type coq_Prod_assert

val bpf_jiffies64_post : ebpf_type coq_Prod_assert

val bpf_jiffies64_spec : ebpf_type funcdes

val bpf_jiffies64_func_dec : nat * (positive * ebpf_type funcdes list)

val bpf_ringbuf_output_pre : ebpf_type coq_Prod_assert

val bpf_ringbuf_output_post : ebpf_type coq_Prod_assert

val bpf_ringbuf_output_spec : ebpf_type funcdes

val bpf_ringbuf_output_func_dec : nat * (positive * ebpf_type funcdes list)

val coq_Helper_func : (nat * ebpf_type funcspec) list
