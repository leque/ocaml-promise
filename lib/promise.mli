(**
   Iterative promise a la Scheme's
   {{:http://srfi.schemers.org/srfi-45/}SRFI-45}
 *)

type 'a t
(**
   [Promise.t] represents deferred computation.
 *)

val of_val : 'a -> 'a t
(**
   [of_val x] creates an already-evaluated promise.
 *)

val of_fun : (unit -> 'a) -> 'a t
(**
   [of_fun f] creates a promise from a thunk.
 *)

val delayed : (unit -> 'a t) -> 'a t
(**
   [delayed] is an another function to create a promise from a thunk.

   You can translate lazy algorithms using [delayed], [of_val], [of_fun],
   and [force] as follows:

   + wrap datatypes with [Promise.t],
   + wrap all constructors with [of_fun (fun () -> ...)] or [of_val],
   + apply [force] before deconstructing values,
   + wrap function bodies with [delayed (fun () -> ...)].
     Cf. \[Wad98\]
     wraps bodies with [Promise.of_fun (fun () -> Promise.force ...)].

   \[Wad98\] Philip Wadler, Walid Taha, and David MacQueen.
   "How to add laziness to a strict language, without even being odd",
   Workshop on Standard ML, Baltimore, September 1998

   For example, a list [filter] function in a hypothetical lazy language
   {[
(* type 'a list = [] | (::) of 'a * 'a list *)

let rec filter p xs =
  match xs with
  | [] -> []
  | x :: xs' ->
    if p x then
      x :: filter p xs'
    else
      filter p xs'
   ]}
   would be translated to a stream (lazy list) [filter] function in OCaml:
   {[
type 'a stream = 'a stream_ Promise.t
and 'a stream_ = Nil | Cons of 'a * 'a stream

let rec filter p xs =
  Promise.delayed (fun () ->
    match Promise.force xs with
    | Nil -> Promise.of_val Nil
    | Cons (x, xs') ->
      if p x then
        Promise.of_fun (fun () -> Cons (x, filter p xs'))
      else
        filter p xs')
   ]}

   This filter function runs in bounded space.
 *)

val force : 'a t -> 'a
(**
   [force x] forces suspension [x] and returns its result.
   If [x] has already been evaluated,
   [force x] returns the result of the last computation
   without re-computing.
   If the computation of [x] raises exception,
   [force x] raises the same exception.
 *)

val is_evaluated : 'a t -> bool
(**
   [is_evaluated x] returns [true] if [x] has already been computed,
   otherwise returns [false].
 *)

val is_val : 'a t -> bool
(**
   [is_val x] returns [true] if [x] has already been computed
   and did not raise an exception.
 *)

val is_exn : 'a t -> bool
(**
   [is_exn x] returns [true] if [x] has already been computed
   and did raise an exception.
 *)

val peek : 'a t -> 'a option
(**
   If [is_val x], [peek x] returns [Some (force x)], otherwise returns [None].
  *)

val map : ('a -> 'b) -> 'a t -> 'b t
(**
   [map f x] maps [f] over the result of [x].
 *)

val return : 'a -> 'a t
(**
   monadic return. same as [of_val].
 *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(**
   monadic bind. [bind x f] is same as [join (map f x)].
 *)

val join : 'a t t -> 'a t
(**
   monadic join. same as [force].
 *)
