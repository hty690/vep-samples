
type coq_Error_message =
| Div_zero
| Memory_leak
| Invalid_load
| Invalid_store
| Invalid_operation
| Invalid_call
| Nofuncdef
| Time_out

type coq_Error =
| Coq_ok
| Coq_error of coq_Error_message
