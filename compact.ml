open Imp.Any

module type Product = sig
  type a
  type b
  type t
  val construct : a code -> b code -> t code
  val deconstruct : t code -> a code * b code
end

implicit module Pair {A: Any} {B: Any}
:  Product
  with type t = A.t * B.t
  and type a = A.t
  and type b = B.t
= struct
  type t = A.t * B.t
  type a = A.t
  type b = B.t
  let construct a b = .< .~a, .~b >.
  let deconstruct p = .< fst .~p >., .< snd .~p >.
end

module type Sum = sig
  type a
  type b
  type t
  val construct_a : a code -> t code
  val construct_b : b code -> t code
  val match_ :
    (a code -> 'r code) ->
    (b code -> 'r code) ->
    t code ->
    'r code
end

implicit module Option {A: Any}
: Sum
  with type t = A.t option
  and type a = A.t
  and type b = unit
= struct
  type t = A.t option
  type a = A.t
  type b = unit
  let construct_a a = .< Some .~a >.
  let construct_b _ = .< None >.
  let match_ fa fb x = .<
    match .~x with
    | Some a -> .~(fa .< a >.)
    | None -> .~(fb .< () >.)
  >.
end

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
    let a = A.decode .< .~n lsr B.bit_width >. in
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
    let encode_a a = .< .~(A.encode a) lsl 1 lor zero >. in
    let encode_b b = .< .~(B.encode b) lsl 1 lor one >. in
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
