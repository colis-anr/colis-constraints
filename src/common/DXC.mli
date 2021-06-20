type t
[@@deriving show]

val false_ : t
val empty : t
(** Alias for {!false}. *)

val singleton : XConstraint.t -> t

val one : XConstraint.t -> t
(** Alias for {!singleton}. *)

val two : XConstraint.t -> XConstraint.t -> t

val and_ : t -> t -> t
val and_l : t list -> t

val or_ : t -> t -> t
val or_l : t list -> t

val quantify : Var.t -> t -> t

val to_seq : t -> XConstraint.t Seq.t
val to_list : t -> XConstraint.t list

val map : (XConstraint.t -> XConstraint.t) -> t -> t
val concat_map : (XConstraint.t -> t) -> t -> t

val from_list : XConstraint.t list -> t
