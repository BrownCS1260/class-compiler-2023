open Compile
open Interp

let difftest (examples : (string * string) list) =
  let results =
    List.map
      (fun (ex, i) -> (compile_and_run_err ex i, interp_err ex i))
      examples
  in
  List.for_all (fun (r1, r2) -> r1 = r2) results

let test () = difftest [("(print (read-num))", "1")]
