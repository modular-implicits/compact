open Imp.Any
open Compact

let thing : (int * int) option = Some (3, 6)

let () = Printf.printf "%d" (size thing)

let () = ignore (module Any_Int : Any)
