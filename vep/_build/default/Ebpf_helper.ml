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

(** val maxint : coq_Z **)

let maxint =
  Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    Coq_xH)))))))))))))))))))))))))))))))

(** val bpf_map_lookup_elem_pre : ebpf_type coq_Prod_assert **)

let bpf_map_lookup_elem_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_lookup_elem_post : ebpf_type coq_Prod_assert **)

let bpf_map_lookup_elem_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: ((Other
    (Obj.magic (Map_element ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), (V_vari
      (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_lookup_elem_spec : ebpf_type funcdes **)

let bpf_map_lookup_elem_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: [])))))); coq_Precon =
    bpf_map_lookup_elem_pre; coq_Postcon = { coq_Assert_r =
    bpf_map_lookup_elem_post; coq_Return = None } }

(** val bpf_map_lookup_elem_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_map_lookup_elem_func_dec =
  (O, (Coq_xH, (bpf_map_lookup_elem_spec :: [])))

(** val bpf_map_update_elem_pre : ebpf_type coq_Prod_assert **)

let bpf_map_update_elem_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_update_elem_post : ebpf_type coq_Prod_assert **)

let bpf_map_update_elem_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_update_elem_spec : ebpf_type funcdes **)

let bpf_map_update_elem_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))) :: []))))))));
    coq_Precon = bpf_map_update_elem_pre; coq_Postcon = { coq_Assert_r =
    bpf_map_update_elem_post; coq_Return = None } }

(** val bpf_map_update_elem_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_map_update_elem_func_dec =
  (O, ((Coq_xO Coq_xH), (bpf_map_update_elem_spec :: [])))

(** val bpf_map_delete_elem_pre : ebpf_type coq_Prod_assert **)

let bpf_map_delete_elem_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: ((Other
    (Obj.magic (Map_element ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (Vlist_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))), (V_vari
      (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: [])) }

(** val bpf_map_delete_elem_post : ebpf_type coq_Prod_assert **)

let bpf_map_delete_elem_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((Ez_val Z0), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = [] }

(** val bpf_map_delete_elem_spec : ebpf_type funcdes **)

let bpf_map_delete_elem_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: [])))))); coq_Precon =
    bpf_map_delete_elem_pre; coq_Postcon = { coq_Assert_r =
    bpf_map_delete_elem_post; coq_Return = None } }

(** val bpf_map_delete_elem_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_map_delete_elem_func_dec =
  (O, ((Coq_xI Coq_xH), (bpf_map_delete_elem_spec :: [])))

(** val bpf_probe_read_pre : ebpf_type coq_Prod_assert **)

let bpf_probe_read_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_probe_read_post : ebpf_type coq_Prod_assert **)

let bpf_probe_read_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_probe_read_spec : ebpf_type funcdes **)

let bpf_probe_read_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: [])))); coq_Precon = bpf_probe_read_pre; coq_Postcon =
    { coq_Assert_r = bpf_probe_read_post; coq_Return = None } }

(** val bpf_probe_read_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_probe_read_func_dec =
  (O, ((Coq_xO (Coq_xO Coq_xH)), (bpf_probe_read_spec :: [])))

(** val bpf_ktime_get_ns_pre : ebpf_type coq_Prod_assert **)

let bpf_ktime_get_ns_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_ktime_get_ns_post : ebpf_type coq_Prod_assert **)

let bpf_ktime_get_ns_post =
  { coq_Prop_list = ((Be (Pvle, (Ez_val Z0), (V_vari (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH))))))) :: []); coq_Local_list = ((Temp (coq_R0,
    r0)) :: []); coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_ktime_get_ns_spec : ebpf_type funcdes **)

let bpf_ktime_get_ns_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_ktime_get_ns_pre;
    coq_Postcon = { coq_Assert_r = bpf_ktime_get_ns_post; coq_Return =
    None } }

(** val bpf_ktime_get_ns_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_ktime_get_ns_func_dec =
  (O, ((Coq_xI (Coq_xO Coq_xH)), (bpf_ktime_get_ns_spec :: [])))

(** val bpf_trace_printk_pre : ebpf_type coq_Prod_assert **)

let bpf_trace_printk_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_trace_printk_post : ebpf_type coq_Prod_assert **)

let bpf_trace_printk_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_trace_printk_spec : ebpf_type funcdes **)

let bpf_trace_printk_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_trace_printk_pre;
    coq_Postcon = { coq_Assert_r = bpf_trace_printk_post; coq_Return =
    None } }

(** val bpf_trace_printk_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_trace_printk_func_dec =
  (O, ((Coq_xO (Coq_xI Coq_xH)), (bpf_trace_printk_spec :: [])))

(** val bpf_get_prandom_u32_pre : ebpf_type coq_Prod_assert **)

let bpf_get_prandom_u32_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_prandom_u32_post : ebpf_type coq_Prod_assert **)

let bpf_get_prandom_u32_post =
  { coq_Prop_list = ((Be (Pvle, (V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), (Ez_val maxint))) :: ((Be (Pvle, (Ez_val Z0), (V_vari
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))) :: [])); coq_Local_list =
    ((Temp (coq_R0, r0)) :: []); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: []);
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_prandom_u32_spec : ebpf_type funcdes **)

let bpf_get_prandom_u32_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_get_prandom_u32_pre;
    coq_Postcon = { coq_Assert_r = bpf_get_prandom_u32_post; coq_Return =
    None } }

(** val bpf_get_prandom_u32_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_get_prandom_u32_func_dec =
  (O, ((Coq_xI (Coq_xI Coq_xH)), (bpf_get_prandom_u32_spec :: [])))

(** val bpf_get_smp_processor_id_pre : ebpf_type coq_Prod_assert **)

let bpf_get_smp_processor_id_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_smp_processor_id_post : ebpf_type coq_Prod_assert **)

let bpf_get_smp_processor_id_post =
  { coq_Prop_list = ((Be (Pvle, (Ez_val Z0), (V_vari (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH))))))) :: []); coq_Local_list = ((Temp (coq_R0,
    r0)) :: []); coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_smp_processor_id_spec : ebpf_type funcdes **)

let bpf_get_smp_processor_id_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_get_smp_processor_id_pre;
    coq_Postcon = { coq_Assert_r = bpf_get_smp_processor_id_post;
    coq_Return = None } }

(** val bpf_get_smp_processor_id_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_get_smp_processor_id_func_dec =
  (O, ((Coq_xO (Coq_xO (Coq_xO Coq_xH))),
    (bpf_get_smp_processor_id_spec :: [])))

(** val bpf_skb_store_bytes_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_store_bytes_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_skb_store_bytes_post : ebpf_type coq_Prod_assert **)

let bpf_skb_store_bytes_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_skb_store_bytes_spec : ebpf_type funcdes **)

let bpf_skb_store_bytes_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_skb_store_bytes_pre;
    coq_Postcon = { coq_Assert_r = bpf_skb_store_bytes_post; coq_Return =
    None } }

(** val bpf_skb_store_bytes_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_store_bytes_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xO Coq_xH))), (bpf_skb_store_bytes_spec :: [])))

(** val bpf_l3_csum_replace_pre : ebpf_type coq_Prod_assert **)

let bpf_l3_csum_replace_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_l3_csum_replace_post : ebpf_type coq_Prod_assert **)

let bpf_l3_csum_replace_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_l3_csum_replace_spec : ebpf_type funcdes **)

let bpf_l3_csum_replace_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_l3_csum_replace_pre;
    coq_Postcon = { coq_Assert_r = bpf_l3_csum_replace_post; coq_Return =
    None } }

(** val bpf_l3_csum_replace_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_l3_csum_replace_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xO Coq_xH))), (bpf_l3_csum_replace_spec :: [])))

(** val bpf_l4_csum_replace_pre : ebpf_type coq_Prod_assert **)

let bpf_l4_csum_replace_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_l4_csum_replace_post : ebpf_type coq_Prod_assert **)

let bpf_l4_csum_replace_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_l4_csum_replace_spec : ebpf_type funcdes **)

let bpf_l4_csum_replace_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_l4_csum_replace_pre;
    coq_Postcon = { coq_Assert_r = bpf_l4_csum_replace_post; coq_Return =
    None } }

(** val bpf_l4_csum_replace_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_l4_csum_replace_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xO Coq_xH))), (bpf_l4_csum_replace_spec :: [])))

(** val bpf_clone_redirect_pre : ebpf_type coq_Prod_assert **)

let bpf_clone_redirect_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_clone_redirect_post : ebpf_type coq_Prod_assert **)

let bpf_clone_redirect_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_clone_redirect_spec : ebpf_type funcdes **)

let bpf_clone_redirect_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))); coq_Precon =
    bpf_clone_redirect_pre; coq_Postcon = { coq_Assert_r =
    bpf_clone_redirect_post; coq_Return = None } }

(** val bpf_clone_redirect_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_clone_redirect_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xI Coq_xH))), (bpf_clone_redirect_spec :: [])))

(** val bpf_get_current_pid_tgid_pre : ebpf_type coq_Prod_assert **)

let bpf_get_current_pid_tgid_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_current_pid_tgid_post : ebpf_type coq_Prod_assert **)

let bpf_get_current_pid_tgid_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_current_pid_tgid_spec : ebpf_type funcdes **)

let bpf_get_current_pid_tgid_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_get_current_pid_tgid_pre;
    coq_Postcon = { coq_Assert_r = bpf_get_current_pid_tgid_post;
    coq_Return = None } }

(** val bpf_get_current_pid_tgid_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_get_current_pid_tgid_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xI Coq_xH))),
    (bpf_get_current_pid_tgid_spec :: [])))

(** val bpf_get_current_uid_gid_pre : ebpf_type coq_Prod_assert **)

let bpf_get_current_uid_gid_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_current_uid_gid_post : ebpf_type coq_Prod_assert **)

let bpf_get_current_uid_gid_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_current_uid_gid_spec : ebpf_type funcdes **)

let bpf_get_current_uid_gid_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_get_current_uid_gid_pre;
    coq_Postcon = { coq_Assert_r = bpf_get_current_uid_gid_post; coq_Return =
    None } }

(** val bpf_get_current_uid_gid_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_get_current_uid_gid_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xI Coq_xH))),
    (bpf_get_current_uid_gid_spec :: [])))

(** val bpf_get_current_comm_pre_1 : ebpf_type coq_Prod_assert **)

let bpf_get_current_comm_pre_1 =
  { coq_Prop_list = ((Be (Pvle, (V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))))) :: []); coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Accessable ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))) :: []))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_current_comm_post_1 : ebpf_type coq_Prod_assert **)

let bpf_get_current_comm_post_1 =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((Ez_val Z0), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Other
    (Obj.magic (Accessable ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))) :: []))));
    coq_Exist_list = [] }

(** val bpf_get_current_comm_spec_1 : ebpf_type funcdes **)

let bpf_get_current_comm_spec_1 =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))); coq_Precon =
    bpf_get_current_comm_pre_1; coq_Postcon = { coq_Assert_r =
    bpf_get_current_comm_post_1; coq_Return = None } }

(** val bpf_get_current_comm_pre_2 : ebpf_type coq_Prod_assert **)

let bpf_get_current_comm_pre_2 =
  { coq_Prop_list = ((In_bound ((Ez_val (Zneg (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))), (Vbop (Oadd,
    (V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), (V_vari (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Ez_val Z0))) :: []);
    coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp (coq_R1, r1)) :: ((Temp
    (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: []))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_current_comm_post_2 : ebpf_type coq_Prod_assert **)

let bpf_get_current_comm_post_2 =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((Ez_val Z0), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: [])));
    coq_Exist_list = [] }

(** val bpf_get_current_comm_spec_2 : ebpf_type funcdes **)

let bpf_get_current_comm_spec_2 =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []));
    coq_Precon = bpf_get_current_comm_pre_2; coq_Postcon = { coq_Assert_r =
    bpf_get_current_comm_post_2; coq_Return = None } }

(** val bpf_get_current_comm_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_get_current_comm_func_dec =
  (O, ((Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))),
    (bpf_get_current_comm_spec_1 :: (bpf_get_current_comm_spec_2 :: []))))

(** val bpf_skb_vlan_push_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_vlan_push_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_vlan_push_post : ebpf_type coq_Prod_assert **)

let bpf_skb_vlan_push_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_vlan_push_spec : ebpf_type funcdes **)

let bpf_skb_vlan_push_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))); coq_Precon =
    bpf_skb_vlan_push_pre; coq_Postcon = { coq_Assert_r =
    bpf_skb_vlan_push_post; coq_Return = None } }

(** val bpf_skb_vlan_push_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_vlan_push_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))),
    (bpf_skb_vlan_push_spec :: [])))

(** val bpf_skb_get_tunnel_key_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_get_tunnel_key_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), PTR_TO_CTX,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_get_tunnel_key_post : ebpf_type coq_Prod_assert **)

let bpf_skb_get_tunnel_key_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), PTR_TO_CTX,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_get_tunnel_key_spec : ebpf_type funcdes **)

let bpf_skb_get_tunnel_key_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: [])))); coq_Precon = bpf_skb_get_tunnel_key_pre;
    coq_Postcon = { coq_Assert_r = bpf_skb_get_tunnel_key_post; coq_Return =
    None } }

(** val bpf_skb_get_tunnel_key_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_get_tunnel_key_func_dec =
  (O, ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))),
    (bpf_skb_get_tunnel_key_spec :: [])))

(** val bpf_skb_set_tunnel_key_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_set_tunnel_key_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), PTR_TO_CTX,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_set_tunnel_key_post : ebpf_type coq_Prod_assert **)

let bpf_skb_set_tunnel_key_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), PTR_TO_CTX,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_set_tunnel_key_spec : ebpf_type funcdes **)

let bpf_skb_set_tunnel_key_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: [])))); coq_Precon = bpf_skb_set_tunnel_key_pre;
    coq_Postcon = { coq_Assert_r = bpf_skb_set_tunnel_key_post; coq_Return =
    None } }

(** val bpf_skb_set_tunnel_key_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_set_tunnel_key_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))),
    (bpf_skb_set_tunnel_key_spec :: [])))

(** val bpf_perf_event_read_pre : ebpf_type coq_Prod_assert **)

let bpf_perf_event_read_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_perf_event_read_post : ebpf_type coq_Prod_assert **)

let bpf_perf_event_read_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_perf_event_read_spec : ebpf_type funcdes **)

let bpf_perf_event_read_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: [])))))); coq_Precon =
    bpf_perf_event_read_pre; coq_Postcon = { coq_Assert_r =
    bpf_perf_event_read_post; coq_Return = None } }

(** val bpf_perf_event_read_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_perf_event_read_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))),
    (bpf_perf_event_read_spec :: [])))

(** val bpf_redirect_pre : ebpf_type coq_Prod_assert **)

let bpf_redirect_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: []))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_redirect_post : ebpf_type coq_Prod_assert **)

let bpf_redirect_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: []))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_redirect_spec : ebpf_type funcdes **)

let bpf_redirect_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: [])))))); coq_Precon =
    bpf_redirect_pre; coq_Postcon = { coq_Assert_r = bpf_redirect_post;
    coq_Return = None } }

(** val bpf_redirect_func_dec : nat * (positive * ebpf_type funcdes list) **)

let bpf_redirect_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (bpf_redirect_spec :: [])))

(** val bpf_perf_event_output_pre : ebpf_type coq_Prod_assert **)

let bpf_perf_event_output_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
      (V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO
      (Coq_xO (Coq_xI (Coq_xI Coq_xH))))), (V_vari (Coq_xI (Coq_xO (Coq_xI
      (Coq_xI Coq_xH))))))))) :: []))))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_perf_event_output_post : ebpf_type coq_Prod_assert **)

let bpf_perf_event_output_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
      (V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO
      (Coq_xO (Coq_xI (Coq_xI Coq_xH))))), (V_vari (Coq_xI (Coq_xO (Coq_xI
      (Coq_xI Coq_xH))))))))) :: []))))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_perf_event_output_spec : ebpf_type funcdes **)

let bpf_perf_event_output_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))) :: ((Coq_xI
    (Coq_xO (Coq_xI (Coq_xI Coq_xH)))) :: []))))))))); coq_Precon =
    bpf_perf_event_output_pre; coq_Postcon = { coq_Assert_r =
    bpf_perf_event_output_post; coq_Return = None } }

(** val bpf_perf_event_output_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_perf_event_output_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))),
    (bpf_perf_event_output_spec :: [])))

(** val bpf_skb_load_bytes_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_load_bytes_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), PTR_TO_CTX,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_load_bytes_post : ebpf_type coq_Prod_assert **)

let bpf_skb_load_bytes_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), PTR_TO_CTX,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_load_bytes_spec_1 : ebpf_type funcdes **)

let bpf_skb_load_bytes_spec_1 =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: [])))); coq_Precon = bpf_skb_load_bytes_pre; coq_Postcon =
    { coq_Assert_r = bpf_skb_load_bytes_post; coq_Return = None } }

(** val bpf_skb_load_bytes_pre_2 : ebpf_type coq_Prod_assert **)

let bpf_skb_load_bytes_pre_2 =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_load_bytes_post_2 : ebpf_type coq_Prod_assert **)

let bpf_skb_load_bytes_post_2 =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_load_bytes_spec_2 : ebpf_type funcdes **)

let bpf_skb_load_bytes_spec_2 =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: [])))); coq_Precon = bpf_skb_load_bytes_pre_2;
    coq_Postcon = { coq_Assert_r = bpf_skb_load_bytes_post_2; coq_Return =
    None } }

(** val bpf_skb_load_bytes_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_load_bytes_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))),
    (bpf_skb_load_bytes_spec_1 :: (bpf_skb_load_bytes_spec_2 :: []))))

(** val bpf_get_stackid_pre : ebpf_type coq_Prod_assert **)

let bpf_get_stackid_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r3)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: []))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_stackid_post : ebpf_type coq_Prod_assert **)

let bpf_get_stackid_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r3)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: []))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_stackid_spec : ebpf_type funcdes **)

let bpf_get_stackid_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))) :: []))))))); coq_Precon = bpf_get_stackid_pre; coq_Postcon =
    { coq_Assert_r = bpf_get_stackid_post; coq_Return = None } }

(** val bpf_get_stackid_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_get_stackid_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))),
    (bpf_get_stackid_spec :: [])))

(** val bpf_csum_diff_pre : ebpf_type coq_Prod_assert **)

let bpf_csum_diff_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_csum_diff_post : ebpf_type coq_Prod_assert **)

let bpf_csum_diff_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_csum_diff_spec : ebpf_type funcdes **)

let bpf_csum_diff_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_csum_diff_pre;
    coq_Postcon = { coq_Assert_r = bpf_csum_diff_post; coq_Return = None } }

(** val bpf_csum_diff_func_dec : nat * (positive * ebpf_type funcdes list) **)

let bpf_csum_diff_func_dec =
  (O, ((Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))),
    (bpf_csum_diff_spec :: [])))

(** val bpf_skb_change_proto_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_change_proto_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_change_proto_post : ebpf_type coq_Prod_assert **)

let bpf_skb_change_proto_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_change_proto_spec : ebpf_type funcdes **)

let bpf_skb_change_proto_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))); coq_Precon =
    bpf_skb_change_proto_pre; coq_Postcon = { coq_Assert_r =
    bpf_skb_change_proto_post; coq_Return = None } }

(** val bpf_skb_change_proto_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_change_proto_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))),
    (bpf_skb_change_proto_spec :: [])))

(** val bpf_skb_under_cgroup_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_under_cgroup_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r3)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: []))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_under_cgroup_post : ebpf_type coq_Prod_assert **)

let bpf_skb_under_cgroup_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r3)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: []))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_skb_under_cgroup_spec : ebpf_type funcdes **)

let bpf_skb_under_cgroup_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))) :: []))))))); coq_Precon = bpf_skb_under_cgroup_pre;
    coq_Postcon = { coq_Assert_r = bpf_skb_under_cgroup_post; coq_Return =
    None } }

(** val bpf_skb_under_cgroup_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_under_cgroup_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
    (bpf_skb_under_cgroup_spec :: [])))

(** val bpf_get_hash_recalc_pre : ebpf_type coq_Prod_assert **)

let bpf_get_hash_recalc_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: [])); coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: []));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_hash_recalc_post : ebpf_type coq_Prod_assert **)

let bpf_get_hash_recalc_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: [])); coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: []));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_hash_recalc_spec : ebpf_type funcdes **)

let bpf_get_hash_recalc_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: []); coq_Precon = bpf_get_hash_recalc_pre; coq_Postcon =
    { coq_Assert_r = bpf_get_hash_recalc_post; coq_Return = None } }

(** val bpf_get_hash_recalc_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_get_hash_recalc_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
    (bpf_get_hash_recalc_spec :: [])))

(** val bpf_get_current_task_pre : ebpf_type coq_Prod_assert **)

let bpf_get_current_task_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_current_task_post : ebpf_type coq_Prod_assert **)

let bpf_get_current_task_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_get_current_task_spec : ebpf_type funcdes **)

let bpf_get_current_task_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_get_current_task_pre;
    coq_Postcon = { coq_Assert_r = bpf_get_current_task_post; coq_Return =
    None } }

(** val bpf_get_current_task_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_get_current_task_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
    (bpf_get_current_task_spec :: [])))

(** val bpf_probe_write_user_pre : ebpf_type coq_Prod_assert **)

let bpf_probe_write_user_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: [])))) }

(** val bpf_probe_write_user_post : ebpf_type coq_Prod_assert **)

let bpf_probe_write_user_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: [])))) }

(** val bpf_probe_write_user_spec : ebpf_type funcdes **)

let bpf_probe_write_user_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_probe_write_user_pre;
    coq_Postcon = { coq_Assert_r = bpf_probe_write_user_post; coq_Return =
    None } }

(** val bpf_probe_write_user_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_probe_write_user_func_dec =
  (O, ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))),
    (bpf_probe_write_user_spec :: [])))

(** val bpf_current_task_under_cgroup_pre : ebpf_type coq_Prod_assert **)

let bpf_current_task_under_cgroup_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_current_task_under_cgroup_post : ebpf_type coq_Prod_assert **)

let bpf_current_task_under_cgroup_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((Vlist_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_current_task_under_cgroup_spec : ebpf_type funcdes **)

let bpf_current_task_under_cgroup_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: [])))))); coq_Precon =
    bpf_current_task_under_cgroup_pre; coq_Postcon = { coq_Assert_r =
    bpf_current_task_under_cgroup_post; coq_Return = None } }

(** val bpf_current_task_under_cgroup_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_current_task_under_cgroup_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))),
    (bpf_current_task_under_cgroup_spec :: [])))

(** val bpf_skb_change_tail_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_change_tail_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: [])))) }

(** val bpf_skb_change_tail_post : ebpf_type coq_Prod_assert **)

let bpf_skb_change_tail_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: [])))) }

(** val bpf_skb_change_tail_spec : ebpf_type funcdes **)

let bpf_skb_change_tail_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_skb_change_tail_pre;
    coq_Postcon = { coq_Assert_r = bpf_skb_change_tail_post; coq_Return =
    None } }

(** val bpf_skb_change_tail_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_change_tail_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))),
    (bpf_skb_change_tail_spec :: [])))

(** val bpf_skb_change_head_pre : ebpf_type coq_Prod_assert **)

let bpf_skb_change_head_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: [])))) }

(** val bpf_skb_change_head_post : ebpf_type coq_Prod_assert **)

let bpf_skb_change_head_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: [])))) }

(** val bpf_skb_change_head_spec : ebpf_type funcdes **)

let bpf_skb_change_head_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_skb_change_head_pre;
    coq_Postcon = { coq_Assert_r = bpf_skb_change_head_post; coq_Return =
    None } }

(** val bpf_skb_change_head_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_skb_change_head_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    (bpf_skb_change_head_spec :: [])))

(** val bpf_xdp_adjust_head_pre : ebpf_type coq_Prod_assert **)

let bpf_xdp_adjust_head_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: []))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))) }

(** val bpf_xdp_adjust_head_post : ebpf_type coq_Prod_assert **)

let bpf_xdp_adjust_head_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: []))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))) }

(** val bpf_xdp_adjust_head_spec : ebpf_type funcdes **)

let bpf_xdp_adjust_head_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_xdp_adjust_head_pre;
    coq_Postcon = { coq_Assert_r = bpf_xdp_adjust_head_post; coq_Return =
    None } }

(** val bpf_xdp_adjust_head_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_xdp_adjust_head_func_dec =
  (O, ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    (bpf_xdp_adjust_head_spec :: [])))

(** val bpf_probe_read_str_pre : ebpf_type coq_Prod_assert **)

let bpf_probe_read_str_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_probe_read_str_post : ebpf_type coq_Prod_assert **)

let bpf_probe_read_str_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_probe_read_str_spec : ebpf_type funcdes **)

let bpf_probe_read_str_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))); coq_Precon =
    bpf_probe_read_str_pre; coq_Postcon = { coq_Assert_r =
    bpf_probe_read_str_post; coq_Return = None } }

(** val bpf_probe_read_str_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_probe_read_str_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    (bpf_probe_read_str_spec :: [])))

(** val bpf_setsockopt_pre : ebpf_type coq_Prod_assert **)

let bpf_setsockopt_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_setsockopt_post : ebpf_type coq_Prod_assert **)

let bpf_setsockopt_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_setsockopt_spec : ebpf_type funcdes **)

let bpf_setsockopt_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_setsockopt_pre;
    coq_Postcon = { coq_Assert_r = bpf_setsockopt_post; coq_Return = None } }

(** val bpf_setsockopt_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_setsockopt_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    (bpf_setsockopt_spec :: [])))

(** val bpf_redirect_map_pre : ebpf_type coq_Prod_assert **)

let bpf_redirect_map_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r3)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: []))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_redirect_map_post : ebpf_type coq_Prod_assert **)

let bpf_redirect_map_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r3)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: []))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_redirect_map_spec : ebpf_type funcdes **)

let bpf_redirect_map_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))) :: []))))))); coq_Precon = bpf_redirect_map_pre; coq_Postcon =
    { coq_Assert_r = bpf_redirect_map_post; coq_Return = None } }

(** val bpf_redirect_map_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_redirect_map_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    (bpf_redirect_map_spec :: [])))

(** val bpf_sock_map_update_pre : ebpf_type coq_Prod_assert **)

let bpf_sock_map_update_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
      (V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO
      (Coq_xO (Coq_xI (Coq_xI Coq_xH))))), (V_vari (Coq_xI (Coq_xO (Coq_xI
      (Coq_xI Coq_xH))))))))) :: [])))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_sock_map_update_post : ebpf_type coq_Prod_assert **)

let bpf_sock_map_update_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
      (V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO
      (Coq_xO (Coq_xI (Coq_xI Coq_xH))))), (V_vari (Coq_xI (Coq_xO (Coq_xI
      (Coq_xI Coq_xH))))))))) :: [])))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_sock_map_update_spec : ebpf_type funcdes **)

let bpf_sock_map_update_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))) :: []))))))));
    coq_Precon = bpf_sock_map_update_pre; coq_Postcon = { coq_Assert_r =
    bpf_sock_map_update_post; coq_Return = None } }

(** val bpf_sock_map_update_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_sock_map_update_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    (bpf_sock_map_update_spec :: [])))

(** val bpf_xdp_adjust_meta_spec : ebpf_type funcdes **)

let bpf_xdp_adjust_meta_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_xdp_adjust_head_pre;
    coq_Postcon = { coq_Assert_r = bpf_xdp_adjust_head_post; coq_Return =
    None } }

(** val bpf_xdp_adjust_meta_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_xdp_adjust_meta_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    (bpf_xdp_adjust_meta_spec :: [])))

(** val bpf_perf_event_read_value_pre : ebpf_type coq_Prod_assert **)

let bpf_perf_event_read_value_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_perf_event_read_value_post : ebpf_type coq_Prod_assert **)

let bpf_perf_event_read_value_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_perf_event_read_value_spec : ebpf_type funcdes **)

let bpf_perf_event_read_value_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))) :: []))))))));
    coq_Precon = bpf_perf_event_read_value_pre; coq_Postcon =
    { coq_Assert_r = bpf_perf_event_read_value_post; coq_Return = None } }

(** val bpf_perf_event_read_value_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_perf_event_read_value_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    (bpf_perf_event_read_value_spec :: [])))

(** val bpf_perf_prog_read_value_pre : ebpf_type coq_Prod_assert **)

let bpf_perf_prog_read_value_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: [])))) }

(** val bpf_perf_prog_read_value_post : ebpf_type coq_Prod_assert **)

let bpf_perf_prog_read_value_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: [])))) }

(** val bpf_perf_prog_read_value_spec : ebpf_type funcdes **)

let bpf_perf_prog_read_value_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_perf_prog_read_value_pre;
    coq_Postcon = { coq_Assert_r = bpf_perf_prog_read_value_post;
    coq_Return = None } }

(** val bpf_perf_prog_read_value_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_perf_prog_read_value_func_dec =
  (O, ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))),
    (bpf_perf_prog_read_value_spec :: [])))

(** val bpf_getsockopt_pre : ebpf_type coq_Prod_assert **)

let bpf_getsockopt_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_getsockopt_post : ebpf_type coq_Prod_assert **)

let bpf_getsockopt_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: ((Temp (coq_R5, r5)) :: [])))))); coq_Sep_list =
    ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: ((Data_at ((V_vari
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r4)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), SCALAR_VALUE, r5)) :: [])))))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: [])))))) }

(** val bpf_getsockopt_spec : ebpf_type funcdes **)

let bpf_getsockopt_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_getsockopt_pre;
    coq_Postcon = { coq_Assert_r = bpf_getsockopt_post; coq_Return = None } }

(** val bpf_getsockopt_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_getsockopt_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))),
    (bpf_getsockopt_spec :: [])))

(** val bpf_override_return_pre : ebpf_type coq_Prod_assert **)

let bpf_override_return_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: []))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_override_return_post : ebpf_type coq_Prod_assert **)

let bpf_override_return_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((Ez_val Z0), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r2)) :: [])));
    coq_Exist_list = [] }

(** val bpf_override_return_spec : ebpf_type funcdes **)

let bpf_override_return_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []));
    coq_Precon = bpf_override_return_pre; coq_Postcon = { coq_Assert_r =
    bpf_override_return_post; coq_Return = None } }

(** val bpf_override_return_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_override_return_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))),
    (bpf_override_return_spec :: [])))

(** val bpf_xdp_adjust_tail_pre : ebpf_type coq_Prod_assert **)

let bpf_xdp_adjust_tail_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: []))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))) }

(** val bpf_xdp_adjust_tail_post : ebpf_type coq_Prod_assert **)

let bpf_xdp_adjust_tail_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: []))); coq_Exist_list = ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))) }

(** val bpf_xdp_adjust_tail_spec : ebpf_type funcdes **)

let bpf_xdp_adjust_tail_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_xdp_adjust_tail_pre;
    coq_Postcon = { coq_Assert_r = bpf_xdp_adjust_tail_post; coq_Return =
    None } }

(** val bpf_xdp_adjust_tail_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_xdp_adjust_tail_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))),
    (bpf_xdp_adjust_tail_spec :: [])))

(** val bpf_fib_lookup_pre : ebpf_type coq_Prod_assert **)

let bpf_fib_lookup_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_fib_lookup_post : ebpf_type coq_Prod_assert **)

let bpf_fib_lookup_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_fib_lookup_spec : ebpf_type funcdes **)

let bpf_fib_lookup_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: [])))); coq_Precon = bpf_fib_lookup_pre; coq_Postcon =
    { coq_Assert_r = bpf_fib_lookup_post; coq_Return = None } }

(** val bpf_fib_lookup_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_fib_lookup_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))),
    (bpf_fib_lookup_spec :: [])))

(** val bpf_map_push_elem_pre : ebpf_type coq_Prod_assert **)

let bpf_map_push_elem_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r3)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: []))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_push_elem_post : ebpf_type coq_Prod_assert **)

let bpf_map_push_elem_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))),
    SCALAR_VALUE, r3)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: []))))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_push_elem_spec : ebpf_type funcdes **)

let bpf_map_push_elem_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))) :: []))))))); coq_Precon = bpf_map_push_elem_pre;
    coq_Postcon = { coq_Assert_r = bpf_map_push_elem_post; coq_Return =
    None } }

(** val bpf_map_push_elem_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_map_push_elem_func_dec =
  (O, ((Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))),
    (bpf_map_push_elem_spec :: [])))

(** val bpf_map_pop_elem_pre : ebpf_type coq_Prod_assert **)

let bpf_map_pop_elem_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_pop_elem_post : ebpf_type coq_Prod_assert **)

let bpf_map_pop_elem_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_pop_elem_spec : ebpf_type funcdes **)

let bpf_map_pop_elem_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: [])))))); coq_Precon =
    bpf_map_pop_elem_pre; coq_Postcon = { coq_Assert_r =
    bpf_map_pop_elem_post; coq_Return = None } }

(** val bpf_map_pop_elem_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_map_pop_elem_func_dec =
  (O, ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))),
    (bpf_map_pop_elem_spec :: [])))

(** val bpf_map_peek_elem_pre : ebpf_type coq_Prod_assert **)

let bpf_map_peek_elem_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_peek_elem_post : ebpf_type coq_Prod_assert **)

let bpf_map_peek_elem_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: []))); coq_Sep_list = ((Data_at
    ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r2)) :: ((Other
    (Obj.magic (Ebpf_map ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO
      Coq_xH))))), (V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
      (V_vari (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xI
      (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (V_vari (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI Coq_xH))))))))) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_map_peek_elem_spec : ebpf_type funcdes **)

let bpf_map_peek_elem_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))) :: [])))))); coq_Precon =
    bpf_map_peek_elem_pre; coq_Postcon = { coq_Assert_r =
    bpf_map_peek_elem_post; coq_Return = None } }

(** val bpf_map_peek_elem_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_map_peek_elem_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))),
    (bpf_map_peek_elem_spec :: [])))

(** val bpf_probe_read_kernel_pre : ebpf_type coq_Prod_assert **)

let bpf_probe_read_kernel_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_probe_read_kernel_post : ebpf_type coq_Prod_assert **)

let bpf_probe_read_kernel_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: []))));
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at ((V_vari (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r1)) :: ((Data_at ((V_vari
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r3)) :: [])))); coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))) :: []) }

(** val bpf_probe_read_kernel_spec : ebpf_type funcdes **)

let bpf_probe_read_kernel_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: []))); coq_Precon =
    bpf_probe_read_kernel_pre; coq_Postcon = { coq_Assert_r =
    bpf_probe_read_kernel_post; coq_Return = None } }

(** val bpf_probe_read_kernel_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_probe_read_kernel_func_dec =
  (O, ((Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))),
    (bpf_probe_read_kernel_spec :: [])))

(** val bpf_jiffies64_pre : ebpf_type coq_Prod_assert **)

let bpf_jiffies64_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_jiffies64_post : ebpf_type coq_Prod_assert **)

let bpf_jiffies64_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: []);
    coq_Sep_list = ((Data_at ((V_vari (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r0)) :: []); coq_Exist_list = ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_jiffies64_spec : ebpf_type funcdes **)

let bpf_jiffies64_spec =
  { coq_Args = []; coq_Param = []; coq_Precon = bpf_jiffies64_pre;
    coq_Postcon = { coq_Assert_r = bpf_jiffies64_post; coq_Return = None } }

(** val bpf_jiffies64_func_dec : nat * (positive * ebpf_type funcdes list) **)

let bpf_jiffies64_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))),
    (bpf_jiffies64_spec :: [])))

(** val bpf_ringbuf_output_pre : ebpf_type coq_Prod_assert **)

let bpf_ringbuf_output_pre =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_ringbuf_output_post : ebpf_type coq_Prod_assert **)

let bpf_ringbuf_output_post =
  { coq_Prop_list = []; coq_Local_list = ((Temp (coq_R0, r0)) :: ((Temp
    (coq_R1, r1)) :: ((Temp (coq_R2, r2)) :: ((Temp (coq_R3, r3)) :: ((Temp
    (coq_R4, r4)) :: []))))); coq_Sep_list = ((Data_at ((V_vari (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE, r0)) :: ((Data_at
    ((V_vari (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), SCALAR_VALUE,
    r1)) :: ((Data_at ((V_vari (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    SCALAR_VALUE, r2)) :: ((Data_at ((V_vari (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))), SCALAR_VALUE, r3)) :: ((Data_at ((V_vari (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))), SCALAR_VALUE, r4)) :: [])))));
    coq_Exist_list = ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: []) }

(** val bpf_ringbuf_output_spec : ebpf_type funcdes **)

let bpf_ringbuf_output_spec =
  { coq_Args = []; coq_Param = ((Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))) :: [])))); coq_Precon = bpf_ringbuf_output_pre; coq_Postcon =
    { coq_Assert_r = bpf_ringbuf_output_post; coq_Return = None } }

(** val bpf_ringbuf_output_func_dec :
    nat * (positive * ebpf_type funcdes list) **)

let bpf_ringbuf_output_func_dec =
  (O, ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (bpf_ringbuf_output_spec :: [])))

(** val coq_Helper_func : (nat * ebpf_type funcspec) list **)

let coq_Helper_func =
  bpf_map_lookup_elem_func_dec :: (bpf_map_update_elem_func_dec :: (bpf_map_delete_elem_func_dec :: (bpf_probe_read_func_dec :: (bpf_ktime_get_ns_func_dec :: (bpf_trace_printk_func_dec :: (bpf_get_prandom_u32_func_dec :: (bpf_get_smp_processor_id_func_dec :: (bpf_skb_store_bytes_func_dec :: (bpf_l3_csum_replace_func_dec :: (bpf_l4_csum_replace_func_dec :: (bpf_clone_redirect_func_dec :: (bpf_get_current_pid_tgid_func_dec :: (bpf_get_current_uid_gid_func_dec :: (bpf_get_current_comm_func_dec :: (bpf_skb_vlan_push_func_dec :: (bpf_skb_get_tunnel_key_func_dec :: (bpf_skb_set_tunnel_key_func_dec :: (bpf_perf_event_read_func_dec :: (bpf_redirect_func_dec :: (bpf_perf_event_output_func_dec :: (bpf_skb_load_bytes_func_dec :: (bpf_get_stackid_func_dec :: (bpf_csum_diff_func_dec :: (bpf_skb_change_proto_func_dec :: (bpf_skb_under_cgroup_func_dec :: (bpf_get_hash_recalc_func_dec :: (bpf_get_current_task_func_dec :: (bpf_probe_write_user_func_dec :: (bpf_current_task_under_cgroup_func_dec :: (bpf_skb_change_tail_func_dec :: (bpf_skb_change_head_func_dec :: (bpf_xdp_adjust_head_func_dec :: (bpf_probe_read_str_func_dec :: (bpf_setsockopt_func_dec :: (bpf_redirect_map_func_dec :: (bpf_sock_map_update_func_dec :: (bpf_xdp_adjust_meta_func_dec :: (bpf_perf_event_read_value_func_dec :: (bpf_perf_prog_read_value_func_dec :: (bpf_getsockopt_func_dec :: (bpf_override_return_func_dec :: (bpf_xdp_adjust_tail_func_dec :: (bpf_fib_lookup_func_dec :: (bpf_map_push_elem_func_dec :: (bpf_map_pop_elem_func_dec :: (bpf_map_peek_elem_func_dec :: (bpf_probe_read_kernel_func_dec :: (bpf_jiffies64_func_dec :: (bpf_ringbuf_output_func_dec :: [])))))))))))))))))))))))))))))))))))))))))))))))))
