(**)

type 'a t = 'a node Promise.t
and 'a node =
  | Nil
  | Cons of 'a * 'a t

(** {2 Basic functions} *)

val nil : unit -> 'a t
val cons : 'a -> 'a t -> 'a t
val xcons : 'a t -> 'a -> 'a t
val singleton : 'a -> 'a t
val uncons : 'a t -> ('a * 'a t) option
val case : 'a t -> nil:(unit -> 'b) -> cons:('a -> 'a t -> 'b) -> 'b
val hd : 'a t -> 'a option
val tl : 'a t -> 'a t option
val is_empty : 'a t -> bool
val nth : 'a t -> int -> 'a option
val length : 'a t -> int
val length_greater_than : int -> 'a t -> bool

(** {2 Folding} *)

val fold_left :
  ('a Promise.t -> 'b -> 'a Promise.t) ->
  'a Promise.t -> 'b t -> 'a Promise.t
val fold_left' : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right :
  ('a -> 'b Promise.t -> 'b Promise.t) ->
  'a t -> 'b Promise.t -> 'b Promise.t
val fold_right' : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(** {2 Unfolding} *)

val unfold_left : ('a -> ('b * 'a) option) -> 'a -> 'b t
val unfold_right : ('a -> ('b * 'a) option) -> 'a -> 'b t

(** {2 *} *)

val append : 'a t -> 'a t -> 'a t
val concat : 'a t t -> 'a t
val rev_append : 'a t -> 'a t -> 'a t
val rev : 'a t -> 'a t

(** {2 *} *)

val map : ('a -> 'b) -> 'a t -> 'b t
val scan_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
val iter : ('a -> unit) -> 'a t -> unit
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val concat_map : ('a -> 'b t) -> 'a t -> 'b t
val exists : ('a -> bool) -> 'a t -> bool
val for_all : ('a -> bool) -> 'a t -> bool
val intersperse : 'a -> 'a t -> 'a t
val intercalate : 'a t -> 'a t t -> 'a t

(** {2 *} *)

val filter : ('a -> bool) -> 'a t -> 'a t
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

(** {2 Zipping & Unzipping} *)

val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val zip : 'a t -> 'b t -> ('a * 'b) t
val unzip : ('a * 'b) t -> 'a t * 'b t

(** {2 Sublists} *)

val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val split_at : int -> 'a t -> 'a t * 'a t
val split_at' : int -> 'a t -> 'a list * 'a t
val take_while : ('a -> bool) -> 'a t -> 'a t
val drop_while : ('a -> bool) -> 'a t -> 'a t
val span : ('a -> bool) -> 'a t -> 'a t * 'a t
val break : ('a -> bool) -> 'a t -> 'a t * 'a t

(** {2 Infinite lists} *)

val from : ?until:int -> ?step:int -> int -> int t
val continually : (unit -> 'a option) -> 'a t
val repeat : 'a -> 'a t
val iterate : ('a -> 'a) -> 'a -> 'a t
val cycle : 'a t -> 'a t

(** {2 Comparing} *)

val is_cyclic : 'a t -> bool
val is_prefix : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val equal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int

(** {2 Conversion from/to lists} *)

val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t

(** {2 Monads} *)

val return : 'a -> 'a t
val join : 'a t t -> 'a t
val bind : ('a -> 'b t) -> 'a t -> 'b t
