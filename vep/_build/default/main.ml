open Test
open Map_sep
open Datatypes
open Assdef
open EbpfExec

let pass_or_not a b = 
  match a, b with
    | (true , []), (true , []) -> "Pass"
    | _, _ -> "Fail" 

let () = 
  let t = Sys.time() in 
  let result = coq_Program_exec coq_Real_tag coq_STATEMENT in
    
  let answer = pass_or_not (coq_No_error_all coq_After_exec)(result) in 
    Printf.printf "Execution time : %fs\n" (Sys.time() -. t);
    print_endline (answer)
