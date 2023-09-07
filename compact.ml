open Staged_generics.Classes

let ((lor), (land), (lsr), (lsl), (lxor), lnot, zero, one) = Int64.
  (logor, logand, shift_right_logical, shift_left, logxor, lognot, of_int 0, of_int 1)

type bits = int64

module type Compact = sig
  type t

  (* must be <= 64 *)
  val bit_width : int

  (* must produce a value with set bits only in the `bit_width` least-significant bits *)
  val encode : t -> bits

  (* must only consider the `bit_width` least-significant bits *)
  val decode : bits -> t
end

module type Compact_ = sig
  type t
  val bit_width : int
  val encode : t code -> bits code
  val decode : bits code -> t code
end

implicit module Compact_Product
  {P: Product}
  {A: Compact_ with type t = P.a}
  {B: Compact_ with type t = P.b}
  : Compact_ with type t = P.t
= struct
  type t = P.t
  let bit_width = A.bit_width + B.bit_width
  let encode x =
    let (a, b) = P.deconstruct x in
    .< .~(A.encode a) lsl B.bit_width lor .~(B.encode b) >.
  let decode n =
    let b = B.decode n in
    let a = A.decode .< Int64.shift_right_logical .~n B.bit_width >. in
    P.construct a b
end

implicit module Compact_Sum
  {S: Sum}
  {A: Compact_ with type t = S.a}
  {B: Compact_ with type t = S.b}
  : Compact_ with type t = S.t
= struct
  type t = S.t
  let bit_width = 1 + max A.bit_width B.bit_width
  let encode x =
    let encode_a (a: S.a code): bits code = .< (.~((A.encode a): bits code): bits) lsl 1 lor zero >. in
    let encode_b (b: S.b code): bits code = .< .~(B.encode b) lsl 1 lor one >. in
    S.match_ encode_a encode_b x
  let decode n =
    let n' = .< .~n lsr 1 >. in .<
    if .~n land one = zero
    then .~(S.construct_a (A.decode n'))
    else .~(S.construct_b (B.decode n'))
  >.
end

(* The __basic__ marker acts as a "stop" for the implicit module resolver.
   Without it, the implicit functors Compact_ and Compact_Basic would form an infinite loop
 *)
module type Compact_Basic = sig
  include Compact
  val __basic__ : unit
end

implicit module Compact_Basic
  {T: Compact_Basic}
  : Compact_ with type t = T.t = struct
  type t = T.t
  let bit_width = T.bit_width
  let encode x = .< T.encode .~x >.
  let decode n = .< T.decode .~n >.
end

implicit module Compact_Unit : Compact_Basic with type t = unit = struct
  type t = unit
  let __basic__ = ()
  let bit_width = 0
  let encode () = zero
  let decode _ = ()
end

implicit module Compact_Char : Compact_Basic with type t = char = struct
  type t = char
  let __basic__ = ()
  let bit_width = 8
  let encode x = Int64.of_int (Char.code x)
  let decode n = Char.chr (Int64.to_int (n land Int64.of_int 255))
end

implicit module Compact_ {S: Compact_} : Compact with type t = S.t = struct
  type t = S.t
  let bit_width = S.bit_width
  let encode x = Runcode.run (S.encode .< x >.)
  let decode n = Runcode.run (S.decode .< n >.)
end

let encode {S: Compact} x = S.encode x
let decode {S: Compact} n = S.decode n
