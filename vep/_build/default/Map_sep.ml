open BinNums
open BinPos
open Datatypes
open List
open Sepdef
open Exprdef
open List_lemma

type ebpf_type =
| NOT_INIT
| SCALAR_VALUE
| PTR_TO_CTX
| CONST_PTR_TO_MAP
| PTR_TO_MAP_VALUE
| PTR_TO_MAP_VALUE_OR_NULL
| PTR_TO_STACK
| PTR_TO_PACKET
| PTR_TO_PACKET_END
| PTR_TO_SOCKET
| PTR_TO_SOCKET_OR_NULL

(** val eqb_ebpf : ebpf_type -> ebpf_type -> bool **)

let eqb_ebpf a b =
  match a with
  | NOT_INIT -> (match b with
                 | NOT_INIT -> true
                 | _ -> false)
  | SCALAR_VALUE -> (match b with
                     | SCALAR_VALUE -> true
                     | _ -> false)
  | PTR_TO_CTX -> (match b with
                   | PTR_TO_CTX -> true
                   | _ -> false)
  | CONST_PTR_TO_MAP -> (match b with
                         | CONST_PTR_TO_MAP -> true
                         | _ -> false)
  | PTR_TO_MAP_VALUE -> (match b with
                         | PTR_TO_MAP_VALUE -> true
                         | _ -> false)
  | PTR_TO_MAP_VALUE_OR_NULL ->
    (match b with
     | PTR_TO_MAP_VALUE_OR_NULL -> true
     | _ -> false)
  | PTR_TO_STACK -> (match b with
                     | PTR_TO_STACK -> true
                     | _ -> false)
  | PTR_TO_PACKET -> (match b with
                      | PTR_TO_PACKET -> true
                      | _ -> false)
  | PTR_TO_PACKET_END -> (match b with
                          | PTR_TO_PACKET_END -> true
                          | _ -> false)
  | PTR_TO_SOCKET -> (match b with
                      | PTR_TO_SOCKET -> true
                      | _ -> false)
  | PTR_TO_SOCKET_OR_NULL ->
    (match b with
     | PTR_TO_SOCKET_OR_NULL -> true
     | _ -> false)

type coq_Ebpf_t =
| Ebpf_map of expr_val * expr_val * expr_val * expr_val * expr_val
| Ebpf_context of expr_val * expr_val * expr_val * expr_val
| Accessable of expr_val * expr_val
| Map_element of expr_val * expr_val * expr_val * expr_val

(** val coq_Ebpf_sep : ebpf_type coq_Separation_def **)

let coq_Ebpf_sep =
  { eqb_ptype = eqb_ebpf; eqb_sep = (fun t1 t2 ->
    match Obj.magic t1 with
    | Ebpf_map (x, id, key, value, size) ->
      (match Obj.magic t2 with
       | Ebpf_map (x', id', key', value', size') ->
         (&&)
           ((&&)
             ((&&) ((&&) (eqb_val x x') (eqb_val id id')) (eqb_val key key'))
             (eqb_val value value')) (eqb_val size size')
       | _ -> false)
    | Ebpf_context (x, data_start, data_end, l) ->
      (match Obj.magic t2 with
       | Ebpf_context (x1, data_start', data_end', l') ->
         (&&)
           ((&&) ((&&) (eqb_val x x1) (eqb_val data_start data_start'))
             (eqb_val data_end data_end')) (eqb_val l l')
       | _ -> false)
    | Accessable (x, len) ->
      (match Obj.magic t2 with
       | Accessable (y, len') -> (&&) (eqb_val x y) (eqb_val len len')
       | _ -> false)
    | Map_element (map_name, key, x, size) ->
      (match Obj.magic t2 with
       | Map_element (map_name', key', x', size') ->
         (&&)
           ((&&) ((&&) (eqb_val map_name map_name') (eqb_val key key'))
             (eqb_val x x')) (eqb_val size size')
       | _ -> false)); eqb_E = (Obj.magic eqb_val); coq_Use_in_other =
    (fun t v ->
    match Obj.magic t with
    | Ebpf_map (x, id, key, value, size) ->
      (||)
        ((||) ((||) ((||) (eqb_val v x) (eqb_val v id)) (eqb_val v key))
          (eqb_val v value)) (eqb_val v size)
    | Ebpf_context (x, data_start, data_end, l) ->
      (||)
        ((||) ((||) (eqb_val v x) (eqb_val v data_start))
          (eqb_val v data_end)) (eqb_val v l)
    | Accessable (x, len) -> (||) (eqb_val v x) (eqb_val v len)
    | Map_element (_, key, x, size) ->
      (||) ((||) (eqb_val v key) (eqb_val v x)) (eqb_val v size));
    coq_Use_in_other_v = (fun t v ->
    match Obj.magic t with
    | Ebpf_map (_, id, key, value, size) ->
      (||) ((||) ((||) (eqb_val v id) (eqb_val v key)) (eqb_val v value))
        (eqb_val v size)
    | Ebpf_context (_, data_start, data_end, _) ->
      (||) (eqb_val v data_start) (eqb_val v data_end)
    | Accessable (_, len) -> eqb_val v len
    | Map_element (_, _, x, _) -> eqb_val v x); coq_Use_in_other_address =
    (fun t v ->
    match Obj.magic t with
    | Ebpf_map (x, _, _, _, _) -> eqb_val v x
    | Ebpf_context (x, _, _, _) -> eqb_val v x
    | Accessable (x, _) -> eqb_val v x
    | Map_element (_, _, x, _) -> eqb_val v x); coq_Changeval_other =
    (fun x y t ->
    match Obj.magic t with
    | Ebpf_map (v, id, key, value, size) ->
      Obj.magic (Ebpf_map ((coq_Changeval x y v), (coq_Changeval x y id),
        (coq_Changeval x y key), (coq_Changeval x y value),
        (coq_Changeval x y size)))
    | Ebpf_context (v, data_start, data_end, l) ->
      Obj.magic (Ebpf_context ((coq_Changeval x y v),
        (coq_Changeval x y data_start), (coq_Changeval x y data_end),
        (coq_Changeval x y l)))
    | Accessable (v, len) ->
      Obj.magic (Accessable ((coq_Changeval x y v), (coq_Changeval x y len)))
    | Map_element (map_name, key, v, size) ->
      Obj.magic (Map_element (map_name, (coq_Changeval x y key),
        (coq_Changeval x y v), (coq_Changeval x y size)))); max_ident_T =
    (fun t ->
    match Obj.magic t with
    | Ebpf_map (x, id, key, value, size) ->
      Pos.max (max_ident_in_expr x)
        (Pos.max (Pos.max (max_ident_in_expr id) (max_ident_in_expr key))
          (Pos.max (max_ident_in_expr value) (max_ident_in_expr size)))
    | Ebpf_context (x, data_start, data_end, l) ->
      Pos.max (max_ident_in_expr l)
        (Pos.max (max_ident_in_expr x)
          (Pos.max (max_ident_in_expr data_start)
            (max_ident_in_expr data_end)))
    | Accessable (x, len) ->
      Pos.max (max_ident_in_expr x) (max_ident_in_expr len)
    | Map_element (_, key, x, size) ->
      Pos.max (max_ident_in_expr key)
        (Pos.max (max_ident_in_expr x) (max_ident_in_expr size))); is_emp =
    (fun t ->
    match Obj.magic t with
    | Ebpf_map (_, _, _, _, size) -> eqb_val size (Ez_val Z0)
    | Ebpf_context (_, data_start, data_end, _) -> eqb_val data_start data_end
    | Accessable (_, _) -> false
    | Map_element (_, _, x, size) ->
      (||) (eqb_val size (Ez_val Z0)) (eqb_val x (Ez_val Z0)));
    coq_Entailment_checker_with_EX = (fun target resource exist_list ->
    match Obj.magic target with
    | Ebpf_map (x, id, key, value, size) ->
      (match resource with
       | Other e ->
         (match Obj.magic e with
          | Ebpf_map (x', id', key', value', size') ->
            if eqb_val x x'
            then let change_list =
                   concat
                     (map (fun a ->
                       match fst a with
                       | V_vari i ->
                         if coq_Find Pos.eqb exist_list i
                         then (i, (snd a)) :: []
                         else []
                       | _ -> []) ((id, id') :: ((key, key') :: ((value,
                       value') :: ((size, size') :: [])))))
                 in
                 { coq_Target_sep = []; coq_Resource_sep = []; coq_EX_list =
                 exist_list; coq_Instantiation_map_list_sep = change_list;
                 coq_Prop_list_sep = []; coq_Proofrule_list = ((Same_cancel
                 resource) :: []) }
            else { coq_Target_sep = ((Other target) :: []);
                   coq_Resource_sep = (resource :: []); coq_EX_list =
                   exist_list; coq_Instantiation_map_list_sep = [];
                   coq_Prop_list_sep = []; coq_Proofrule_list = [] }
          | _ ->
            { coq_Target_sep = ((Other target) :: []); coq_Resource_sep =
              (resource :: []); coq_EX_list = exist_list;
              coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
              coq_Proofrule_list = [] })
       | _ ->
         { coq_Target_sep = ((Other target) :: []); coq_Resource_sep =
           (resource :: []); coq_EX_list = exist_list;
           coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
           coq_Proofrule_list = [] })
    | Ebpf_context (x, data_start, data_end, l) ->
      (match resource with
       | Other e ->
         (match Obj.magic e with
          | Ebpf_context (x1, data_start1, data_end1, l') ->
            if (&&)
                 ((&&) ((&&) (eqb_val x x1) (eqb_val data_start data_start1))
                   (eqb_val data_end data_end1)) (eqb_val l l')
            then { coq_Target_sep = []; coq_Resource_sep = []; coq_EX_list =
                   exist_list; coq_Instantiation_map_list_sep = [];
                   coq_Prop_list_sep = []; coq_Proofrule_list = ((Same_cancel
                   resource) :: []) }
            else { coq_Target_sep = ((Other target) :: []);
                   coq_Resource_sep = (resource :: []); coq_EX_list =
                   exist_list; coq_Instantiation_map_list_sep = [];
                   coq_Prop_list_sep = []; coq_Proofrule_list = [] }
          | _ ->
            { coq_Target_sep = ((Other target) :: []); coq_Resource_sep =
              (resource :: []); coq_EX_list = exist_list;
              coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
              coq_Proofrule_list = [] })
       | _ ->
         { coq_Target_sep = ((Other target) :: []); coq_Resource_sep =
           (resource :: []); coq_EX_list = exist_list;
           coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
           coq_Proofrule_list = [] })
    | Accessable (x, len) ->
      (match len with
       | V_vari i ->
         if coq_Find Pos.eqb exist_list i
         then (match resource with
               | Other e ->
                 (match Obj.magic e with
                  | Accessable (x1, len1) ->
                    if eqb_val x x1
                    then { coq_Target_sep = []; coq_Resource_sep = [];
                           coq_EX_list = exist_list;
                           coq_Instantiation_map_list_sep = ((i,
                           len1) :: []); coq_Prop_list_sep = [];
                           coq_Proofrule_list = [] }
                    else { coq_Target_sep = ((Other target) :: []);
                           coq_Resource_sep = (resource :: []); coq_EX_list =
                           exist_list; coq_Instantiation_map_list_sep = [];
                           coq_Prop_list_sep = []; coq_Proofrule_list = [] }
                  | _ ->
                    { coq_Target_sep = ((Other target) :: []);
                      coq_Resource_sep = (resource :: []); coq_EX_list =
                      exist_list; coq_Instantiation_map_list_sep = [];
                      coq_Prop_list_sep = []; coq_Proofrule_list = [] })
               | _ ->
                 { coq_Target_sep = ((Other target) :: []);
                   coq_Resource_sep = (resource :: []); coq_EX_list =
                   exist_list; coq_Instantiation_map_list_sep = [];
                   coq_Prop_list_sep = []; coq_Proofrule_list = [] })
         else (match resource with
               | Other e ->
                 (match Obj.magic e with
                  | Accessable (x1, len1) ->
                    if (&&) (eqb_val x x1) (eqb_val len len1)
                    then { coq_Target_sep = []; coq_Resource_sep = [];
                           coq_EX_list = exist_list;
                           coq_Instantiation_map_list_sep = [];
                           coq_Prop_list_sep = []; coq_Proofrule_list =
                           ((Same_cancel resource) :: []) }
                    else { coq_Target_sep = ((Other target) :: []);
                           coq_Resource_sep = (resource :: []); coq_EX_list =
                           exist_list; coq_Instantiation_map_list_sep = [];
                           coq_Prop_list_sep = []; coq_Proofrule_list = [] }
                  | _ ->
                    { coq_Target_sep = ((Other target) :: []);
                      coq_Resource_sep = (resource :: []); coq_EX_list =
                      exist_list; coq_Instantiation_map_list_sep = [];
                      coq_Prop_list_sep = []; coq_Proofrule_list = [] })
               | _ ->
                 { coq_Target_sep = ((Other target) :: []);
                   coq_Resource_sep = (resource :: []); coq_EX_list =
                   exist_list; coq_Instantiation_map_list_sep = [];
                   coq_Prop_list_sep = []; coq_Proofrule_list = [] })
       | _ ->
         (match resource with
          | Other e ->
            (match Obj.magic e with
             | Accessable (x1, len1) ->
               if (&&) (eqb_val x x1) (eqb_val len len1)
               then { coq_Target_sep = []; coq_Resource_sep = [];
                      coq_EX_list = exist_list;
                      coq_Instantiation_map_list_sep = [];
                      coq_Prop_list_sep = []; coq_Proofrule_list =
                      ((Same_cancel resource) :: []) }
               else { coq_Target_sep = ((Other target) :: []);
                      coq_Resource_sep = (resource :: []); coq_EX_list =
                      exist_list; coq_Instantiation_map_list_sep = [];
                      coq_Prop_list_sep = []; coq_Proofrule_list = [] }
             | _ ->
               { coq_Target_sep = ((Other target) :: []); coq_Resource_sep =
                 (resource :: []); coq_EX_list = exist_list;
                 coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
                 coq_Proofrule_list = [] })
          | _ ->
            { coq_Target_sep = ((Other target) :: []); coq_Resource_sep =
              (resource :: []); coq_EX_list = exist_list;
              coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
              coq_Proofrule_list = [] }))
    | Map_element (map_name, key, x, size) ->
      (match resource with
       | Other e ->
         (match Obj.magic e with
          | Map_element (map_name', key', x1, size1) ->
            if (&&) (eqb_val map_name map_name') (eqb_val key key')
            then (match x with
                  | V_vari i ->
                    if coq_Find Pos.eqb exist_list i
                    then (match size with
                          | V_vari i' ->
                            if coq_Find Pos.eqb exist_list i'
                            then { coq_Target_sep = []; coq_Resource_sep =
                                   []; coq_EX_list = exist_list;
                                   coq_Instantiation_map_list_sep = ((i,
                                   x1) :: ((i', size1) :: []));
                                   coq_Prop_list_sep = [];
                                   coq_Proofrule_list = ((Same_cancel
                                   resource) :: []) }
                            else { coq_Target_sep = []; coq_Resource_sep =
                                   []; coq_EX_list = exist_list;
                                   coq_Instantiation_map_list_sep = ((i,
                                   x1) :: []); coq_Prop_list_sep = [];
                                   coq_Proofrule_list = ((Same_cancel
                                   resource) :: []) }
                          | _ ->
                            { coq_Target_sep = []; coq_Resource_sep = [];
                              coq_EX_list = exist_list;
                              coq_Instantiation_map_list_sep = ((i,
                              x1) :: []); coq_Prop_list_sep = [];
                              coq_Proofrule_list = ((Same_cancel
                              resource) :: []) })
                    else (match size with
                          | V_vari i' ->
                            if coq_Find Pos.eqb exist_list i'
                            then { coq_Target_sep = []; coq_Resource_sep =
                                   []; coq_EX_list = exist_list;
                                   coq_Instantiation_map_list_sep = ((i',
                                   size1) :: []); coq_Prop_list_sep = [];
                                   coq_Proofrule_list = ((Same_cancel
                                   resource) :: []) }
                            else { coq_Target_sep = []; coq_Resource_sep =
                                   []; coq_EX_list = exist_list;
                                   coq_Instantiation_map_list_sep = [];
                                   coq_Prop_list_sep = [];
                                   coq_Proofrule_list = ((Same_cancel
                                   resource) :: []) }
                          | _ ->
                            { coq_Target_sep = []; coq_Resource_sep = [];
                              coq_EX_list = exist_list;
                              coq_Instantiation_map_list_sep = [];
                              coq_Prop_list_sep = []; coq_Proofrule_list =
                              ((Same_cancel resource) :: []) })
                  | Vlist_vari i ->
                    if coq_Find Pos.eqb exist_list i
                    then (match size with
                          | V_vari i' ->
                            if coq_Find Pos.eqb exist_list i'
                            then { coq_Target_sep = []; coq_Resource_sep =
                                   []; coq_EX_list = exist_list;
                                   coq_Instantiation_map_list_sep = ((i,
                                   x1) :: ((i', size1) :: []));
                                   coq_Prop_list_sep = [];
                                   coq_Proofrule_list = ((Same_cancel
                                   resource) :: []) }
                            else { coq_Target_sep = []; coq_Resource_sep =
                                   []; coq_EX_list = exist_list;
                                   coq_Instantiation_map_list_sep = ((i,
                                   x1) :: []); coq_Prop_list_sep = [];
                                   coq_Proofrule_list = ((Same_cancel
                                   resource) :: []) }
                          | _ ->
                            { coq_Target_sep = []; coq_Resource_sep = [];
                              coq_EX_list = exist_list;
                              coq_Instantiation_map_list_sep = ((i,
                              x1) :: []); coq_Prop_list_sep = [];
                              coq_Proofrule_list = ((Same_cancel
                              resource) :: []) })
                    else (match size with
                          | V_vari i' ->
                            if coq_Find Pos.eqb exist_list i'
                            then { coq_Target_sep = []; coq_Resource_sep =
                                   []; coq_EX_list = exist_list;
                                   coq_Instantiation_map_list_sep = ((i',
                                   size1) :: []); coq_Prop_list_sep = [];
                                   coq_Proofrule_list = ((Same_cancel
                                   resource) :: []) }
                            else { coq_Target_sep = []; coq_Resource_sep =
                                   []; coq_EX_list = exist_list;
                                   coq_Instantiation_map_list_sep = [];
                                   coq_Prop_list_sep = [];
                                   coq_Proofrule_list = ((Same_cancel
                                   resource) :: []) }
                          | _ ->
                            { coq_Target_sep = []; coq_Resource_sep = [];
                              coq_EX_list = exist_list;
                              coq_Instantiation_map_list_sep = [];
                              coq_Prop_list_sep = []; coq_Proofrule_list =
                              ((Same_cancel resource) :: []) })
                  | _ ->
                    (match size with
                     | V_vari i' ->
                       if coq_Find Pos.eqb exist_list i'
                       then { coq_Target_sep = []; coq_Resource_sep = [];
                              coq_EX_list = exist_list;
                              coq_Instantiation_map_list_sep = ((i',
                              size1) :: []); coq_Prop_list_sep = [];
                              coq_Proofrule_list = ((Same_cancel
                              resource) :: []) }
                       else { coq_Target_sep = []; coq_Resource_sep = [];
                              coq_EX_list = exist_list;
                              coq_Instantiation_map_list_sep = [];
                              coq_Prop_list_sep = []; coq_Proofrule_list =
                              ((Same_cancel resource) :: []) }
                     | _ ->
                       { coq_Target_sep = []; coq_Resource_sep = [];
                         coq_EX_list = exist_list;
                         coq_Instantiation_map_list_sep = [];
                         coq_Prop_list_sep = []; coq_Proofrule_list =
                         ((Same_cancel resource) :: []) }))
            else { coq_Target_sep = ((Other target) :: []);
                   coq_Resource_sep = (resource :: []); coq_EX_list =
                   exist_list; coq_Instantiation_map_list_sep = [];
                   coq_Prop_list_sep = []; coq_Proofrule_list = [] }
          | _ ->
            { coq_Target_sep = ((Other target) :: []); coq_Resource_sep =
              (resource :: []); coq_EX_list = exist_list;
              coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
              coq_Proofrule_list = [] })
       | _ ->
         { coq_Target_sep = ((Other target) :: []); coq_Resource_sep =
           (resource :: []); coq_EX_list = exist_list;
           coq_Instantiation_map_list_sep = []; coq_Prop_list_sep = [];
           coq_Proofrule_list = [] })); coq_Sep_simplify_prechecker =
    (fun _ -> None); coq_Get_prop = (fun _ _ -> []); coq_Get_simpl_expr_sep =
    (fun _ -> ([], None)); coq_Get_simpl_expr_data_at = (fun _ _ _ -> None);
    coq_Sep_pure_fact_prechecker = (fun _ _ _ -> None); coq_Compare_sep =
    (fun check_expr sep ->
    match sep with
    | Data_at (_, _, addr) ->
      (match addr with
       | Vfield_address (addr', _) ->
         if eqb_val addr' (Obj.magic check_expr) then Coq_succ else Coq_fail
       | _ -> Coq_fail)
    | _ -> Coq_fail); coq_Sep_split_prechecker = (fun _ -> None);
    coq_Sep_split_helper = (fun sepT _ _ -> (((Other sepT) :: []), [])) }

type coq_Ebpf_Sep = (ebpf_type, coq_Ebpf_t) coq_Separation
