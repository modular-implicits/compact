open Generics

let ((lor), (land), (asr), (lsr), (lsl), (lxor), lnot, zero, one) = Int64.
  (logor, logand, shift_right, shift_right_logical, shift_left, logxor, lognot, of_int 0, of_int 1)

module type Compact = sig
  type t

  (* must return a number between 0 and 2^bit_width *)
  val compress : t -> int64

  (* must only depend on the 2^bit_width least significant bits *)
  val decompress : int64 -> t

  (* TODO: is int64 overflow defined behaviour in ocaml? *)
  (* must be <= 64 *)
  val bit_width : int
end

implicit module Compact_Char: Compact with type t = char = struct
  type t = char
  let compress x = Int64.of_int (Char.code x)
  let decompress x = Char.chr (Int64.to_int x)
  let bit_width = 8
end

implicit module Compact_Unit : Compact with type t = unit
= struct
  type t = unit
  let compress () = zero
  let decompress _ = ()
  let bit_width = 0
end

implicit module Compact_Sum {X: Compact} {Y: Compact} :
  Compact with type t = (X.t, Y.t) Generic.sum
= struct
  type t = (X.t, Y.t) Generic.sum

  (* note: if x and y don't have the same bit width, we just implicitly left-pad with 0s
     e.g. if x is 5 bits wide and y is 7 bits wide, then:

     Left x  -> 00xxxxx0
     Right y -> yyyyyyy1
   *)

  let compress = function
    | Generic.Left x -> X.compress x lsl 1 lor zero
    | Generic.Right y -> Y.compress y lsl 1 lor one

  let decompress c =
    let flag = c land one in
    let c' = c lsr 1 in
    if flag = zero
    then Generic.Left (X.decompress c')
    else Generic.Right (Y.decompress c')

  let bit_width = 1 + max X.bit_width Y.bit_width
end

implicit module Compact_Prod {X: Compact} {Y: Compact} :
  Compact with type t = (X.t, Y.t) Generic.prod
= struct
  type t = (X.t, Y.t) Generic.prod

  let compress (Generic.Prod (x, y)) =
    X.compress x lsl Y.bit_width lor Y.compress y

  let decompress c =
    Generic.Prod (X.decompress (c lsr Y.bit_width), Y.decompress c)

  let bit_width = X.bit_width + Y.bit_width
end

implicit module Compact_Basic {X: Compact} :
  Compact with type t = X.t Generic.basic
= struct
  type t = X.t Generic.basic
  let compress (Generic.Basic (_, x)) = X.compress x
  (* TODO: what to do about the label here? *)
  let decompress c = Generic.Basic ("%decompressed", (X.decompress c))
  let bit_width = X.bit_width
end

(* not marked implicit to avoid overlapping instances
   You can opt-in if you want to use this, by writing:
    ```
    let implicit module Compact_Generic = Compact_Generic in
    (* ... *)
    ```
 *)
module Compact_Generic {X: Generic.Generic} {XRep: Compact with type t = X.rep}
  : Compact with type t = X.t
= struct
  type t = X.t
  let compress x = XRep.compress (X.toRep x)
  let decompress c = X.fromRep (XRep.decompress c)
  let bit_width = XRep.bit_width
end
