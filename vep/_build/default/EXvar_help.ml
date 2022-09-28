open BinNums
open BinPos
open Localdef
open Propdef
open Exprdef
open List_lemma

(** val max_ident : ident list -> ident **)

let rec max_ident = function
| [] -> Coq_xH
| a :: l' -> Pos.max a (max_ident l')

(** val coq_Rename : expr_val -> (ident * ident) list -> expr_val **)

let rec coq_Rename val0 f =
  match val0 with
  | V_vari c -> (match look_up f c with
                 | Some a -> V_vari a
                 | None -> V_vari c)
  | Vfield_address (address, name) ->
    Vfield_address ((coq_Rename address f), name)
  | Ez_val _ -> val0
  | Vuop (op, v) -> Vuop (op, (coq_Rename v f))
  | Vbop (op, v1, v2) -> Vbop (op, (coq_Rename v1 f), (coq_Rename v2 f))
  | Vlop (op, v1, v2) -> Vlop (op, (coq_Rename v1 f), (coq_Rename v2 f))
  | Vlist_vari c ->
    (match look_up f c with
     | Some a -> Vlist_vari a
     | None -> Vlist_vari c)
  | Vlist_sublist (l, r, s) ->
    Vlist_sublist ((coq_Rename l f), (coq_Rename r f), (coq_Rename s f))
  | Vlist_length s -> Vlist_length (coq_Rename s f)
  | Vif (a, b, c) ->
    Vif ((coq_Rename a f), (coq_Rename b f), (coq_Rename c f))

(** val coq_Rename_option :
    expr_val option -> (ident * ident) list -> expr_val option **)

let coq_Rename_option val0 f =
  match val0 with
  | Some v -> Some (coq_Rename v f)
  | None -> None

(** val coq_Rename_Proposition :
    coq_Proposition -> (ident * ident) list -> coq_Proposition **)

let rec coq_Rename_Proposition s f =
  match s with
  | Up p -> Up (coq_Rename_Proposition p f)
  | Bp (op, p1, p2) ->
    Bp (op, (coq_Rename_Proposition p1 f), (coq_Rename_Proposition p2 f))
  | Ue (op, e) -> Ue (op, (coq_Rename e f))
  | Be (op, e1, e2) -> Be (op, (coq_Rename e1 f), (coq_Rename e2 f))
  | In_bound (e1, e2, e3) ->
    In_bound ((coq_Rename e1 f), (coq_Rename e2 f), (coq_Rename e3 f))
  | Qf (op, e, p) -> Qf (op, (coq_Rename e f), (coq_Rename_Proposition p f))
  | _ -> s

(** val coq_Rename_Local : coq_Local -> (ident * ident) list -> coq_Local **)

let coq_Rename_Local p f =
  let Temp (id, v) = p in Temp (id, (coq_Rename v f))

(** val coq_Rename_Proposition_list :
    coq_Proposition list -> (ident * ident) list -> coq_Proposition list **)

let rec coq_Rename_Proposition_list propx_list f =
  match propx_list with
  | [] -> []
  | p :: l' ->
    (coq_Rename_Proposition p f) :: (coq_Rename_Proposition_list l' f)

(** val coq_Rename_Local_list :
    coq_Local list -> (ident * ident) list -> coq_Local list **)

let rec coq_Rename_Local_list localx_list f =
  match localx_list with
  | [] -> []
  | p :: l' -> (coq_Rename_Local p f) :: (coq_Rename_Local_list l' f)

(** val coq_Rename_ident :
    ident list -> (ident * ident) list -> ident list **)

let rec coq_Rename_ident l f =
  match l with
  | [] -> []
  | c :: l' ->
    (match look_up f c with
     | Some a -> a
     | None -> c) :: (coq_Rename_ident l' f)

(** val coq_Find_ident_val : ident -> expr_val -> bool **)

let rec coq_Find_ident_val i = function
| V_vari i' -> Pos.eqb i' i
| Vfield_address (address, _) -> coq_Find_ident_val i address
| Ez_val _ -> false
| Vuop (_, v) -> coq_Find_ident_val i v
| Vbop (_, v1, v2) -> (||) (coq_Find_ident_val i v1) (coq_Find_ident_val i v2)
| Vlop (_, v1, v2) -> (||) (coq_Find_ident_val i v1) (coq_Find_ident_val i v2)
| Vlist_vari i' -> Pos.eqb i' i
| Vlist_sublist (l, r, s) ->
  (||) ((||) (coq_Find_ident_val i l) (coq_Find_ident_val i r))
    (coq_Find_ident_val i s)
| Vlist_length s -> coq_Find_ident_val i s
| Vif (a, b, c) ->
  (||) ((||) (coq_Find_ident_val i a) (coq_Find_ident_val i b))
    (coq_Find_ident_val i c)

(** val coq_Find_ident_l : ident -> coq_Local -> bool **)

let coq_Find_ident_l i = function
| Temp (_, y) -> coq_Find_ident_val i y

(** val coq_Find_ident_L : ident -> coq_Local list -> bool **)

let rec coq_Find_ident_L i = function
| [] -> false
| l :: l0 -> (||) (coq_Find_ident_l i l) (coq_Find_ident_L i l0)

(** val coq_Find_ident_p : ident -> coq_Proposition -> bool **)

let rec coq_Find_ident_p i = function
| Up p0 -> coq_Find_ident_p i p0
| Bp (_, p1, p2) -> (||) (coq_Find_ident_p i p1) (coq_Find_ident_p i p2)
| Ue (_, e) -> coq_Find_ident_val i e
| Be (_, e1, e2) -> (||) (coq_Find_ident_val i e1) (coq_Find_ident_val i e2)
| In_bound (low, val0, high) ->
  (||) ((||) (coq_Find_ident_val i low) (coq_Find_ident_val i val0))
    (coq_Find_ident_val i high)
| Qf (_, e, p0) -> (||) (coq_Find_ident_val i e) (coq_Find_ident_p i p0)
| _ -> false

(** val coq_Find_ident_P : ident -> coq_Proposition list -> bool **)

let rec coq_Find_ident_P i = function
| [] -> false
| l :: l0 -> (||) (coq_Find_ident_p i l) (coq_Find_ident_P i l0)
