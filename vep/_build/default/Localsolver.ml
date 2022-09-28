open BinPos
open Datatypes
open Localdef
open Propdef
open Exprdef
open List_lemma

type coq_Local_solver_return = { coq_Localx : (coq_Local * coq_Local) option;
                                 coq_Instantiation_map : (ident * expr_val)
                                                         list;
                                 coq_PropL : coq_Proposition list }

type coq_Local_list_helper_return = { coq_Targetx : coq_Local option;
                                      coq_Resourcex : coq_Local list;
                                      coq_Instantiation_map' : (ident * expr_val)
                                                               list;
                                      coq_PropL' : coq_Proposition list }

type coq_Local_list_solver_return = { coq_Target_local : coq_Local list;
                                      coq_Resource_local : coq_Local list;
                                      coq_Instantiation_map_list_local : 
                                      (ident * expr_val) list;
                                      coq_Prop_list_local : coq_Proposition
                                                            list }

(** val coq_Local_solver_with_EX :
    coq_Local -> coq_Local -> ident list -> coq_Local_solver_return **)

let coq_Local_solver_with_EX target resource exist_list =
  let Temp (idG, vG) = target in
  let Temp (idR, vR) = resource in
  if Pos.eqb idG idR
  then (match vG with
        | V_vari cG ->
          if coq_Find Pos.eqb exist_list cG
          then { coq_Localx = None; coq_Instantiation_map = ((cG, vR) :: []);
                 coq_PropL = [] }
          else { coq_Localx = None; coq_Instantiation_map = []; coq_PropL =
                 ((Be (Pvequal, vG, vR)) :: []) }
        | _ ->
          { coq_Localx = None; coq_Instantiation_map = []; coq_PropL = ((Be
            (Pvequal, vG, vR)) :: []) })
  else { coq_Localx = (Some (target, resource)); coq_Instantiation_map = [];
         coq_PropL = [] }

(** val coq_Local_list_solver_with_EX_helper :
    coq_Local -> coq_Local list -> ident list -> nat ->
    coq_Local_list_helper_return **)

let rec coq_Local_list_solver_with_EX_helper target resourceL exist_list = function
| O ->
  { coq_Targetx = None; coq_Resourcex = resourceL; coq_Instantiation_map' =
    []; coq_PropL' = [] }
| S fuel' ->
  (match resourceL with
   | [] ->
     { coq_Targetx = None; coq_Resourcex = []; coq_Instantiation_map' = [];
       coq_PropL' = [] }
   | xR :: lR' ->
     let l_return = coq_Local_solver_with_EX target xR exist_list in
     (match l_return.coq_Localx with
      | Some p ->
        let (target', resource') = p in
        let l_helper_return =
          coq_Local_list_solver_with_EX_helper target' lR' exist_list fuel'
        in
        { coq_Targetx = l_helper_return.coq_Targetx; coq_Resourcex =
        (resource' :: l_helper_return.coq_Resourcex);
        coq_Instantiation_map' =
        (app l_return.coq_Instantiation_map
          l_helper_return.coq_Instantiation_map'); coq_PropL' =
        (app l_return.coq_PropL l_helper_return.coq_PropL') }
      | None ->
        { coq_Targetx = None; coq_Resourcex = lR'; coq_Instantiation_map' =
          l_return.coq_Instantiation_map; coq_PropL' = l_return.coq_PropL }))

(** val coq_Local_list_solver_with_EX :
    coq_Local list -> coq_Local list -> ident list ->
    coq_Local_list_solver_return **)

let rec coq_Local_list_solver_with_EX targetL resourceL exist_list =
  match targetL with
  | [] ->
    { coq_Target_local = targetL; coq_Resource_local = resourceL;
      coq_Instantiation_map_list_local = []; coq_Prop_list_local = [] }
  | xG :: lG' ->
    let l_helper_return =
      coq_Local_list_solver_with_EX_helper xG resourceL exist_list
        (length resourceL)
    in
    let l_list_return =
      coq_Local_list_solver_with_EX lG' l_helper_return.coq_Resourcex
        exist_list
    in
    (match l_helper_return.coq_Targetx with
     | Some target' ->
       { coq_Target_local = (target' :: l_list_return.coq_Target_local);
         coq_Resource_local = l_list_return.coq_Resource_local;
         coq_Instantiation_map_list_local =
         (app l_helper_return.coq_Instantiation_map'
           l_list_return.coq_Instantiation_map_list_local);
         coq_Prop_list_local =
         (app l_helper_return.coq_PropL' l_list_return.coq_Prop_list_local) }
     | None ->
       { coq_Target_local = l_list_return.coq_Target_local;
         coq_Resource_local = l_list_return.coq_Resource_local;
         coq_Instantiation_map_list_local =
         (app l_helper_return.coq_Instantiation_map'
           l_list_return.coq_Instantiation_map_list_local);
         coq_Prop_list_local =
         (app l_helper_return.coq_PropL' l_list_return.coq_Prop_list_local) })
