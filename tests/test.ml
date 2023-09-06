open Imp.Any
open Compact
open implicit Staged_generics.Instances

let thing : (char * char) option = Some ('a', 'x')

let () = Printf.printf "%d\n" (Int64.to_int (encode thing))

let () = assert (decode (encode thing) = thing)

let () = ignore (module Any_Int : Any)
