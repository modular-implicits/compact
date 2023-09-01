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

module type Sized = sig
  (* Compute some notion of "size", in arbitrary units.
     This will bare little relation to the actual amount of memory the object takes up.
   *)
  type t
  val size : t -> int
end

module type Sized_ = sig
  type t
  val size : t code -> int code
end

implicit module Sized_Product
  {P: Product}
  {A: Sized_ with type t = P.a}
  {B: Sized_ with type t = P.b}
  : Sized_ with type t = P.t
= struct
  type t = P.t
  let size x =
    let (a, b) = P.deconstruct x in
    .< .~(A.size a) + .~(B.size b) >.
end

implicit module Sized_Sum
  {S: Sum}
  {A: Sized_ with type t = S.a}
  {B: Sized_ with type t = S.b}
  : Sized_ with type t = S.t
= struct
  type t = S.t
  let size x = 
    .< 1 + .~(S.match_ A.size B.size x) >.
end

(* The __basic__ marker acts as a "stop" for the implicit module resolver.
   Without it, the implicit functors Sized_ and Sized_Basic would form an infinite loop
 *)
module type Sized_Basic = sig
  include Sized
  val __basic__ : unit
end

implicit module Sized_Basic
  {T: Sized_Basic}
  : Sized_ with type t = T.t = struct
  type t = T.t
  let size x = .< T.size .~x >.
end

implicit module Sized_Unit : Sized_Basic with type t = unit = struct
  type t = unit
  let __basic__ = ()
  let size () = 1
end

implicit module Sized_Int : Sized_Basic with type t = int = struct
  type t = int
  let __basic__ = ()
  let size _ = 4
end

implicit module Sized_List {S: Sized} : Sized_Basic with type t = S.t list = struct
  type t = S.t list
  let __basic__ = ()
  (* until we have some way of dealing with inductive types, this will have to do *)
  let rec size = function
    | [] -> 1
    | x :: xs -> 1 + S.size x + size xs
end

implicit module Sized_ {S: Sized_} : Sized with type t = S.t = struct
  type t = S.t
  let size x = Runcode.run (S.size .< x >.)
end

let size {S: Sized} x = S.size x
