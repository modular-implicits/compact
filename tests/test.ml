open Imp.Any
open Compact

let thing : (int * int) option = Some (0, 0)

let () = Printf.printf "%d\n" (size thing)

let () = ignore (module Any_Int : Any)
