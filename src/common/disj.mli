type t
[@@deriving show]

val false_ : t
val empty : t
(** Alias for {!false}. *)

val singleton : Conj.t -> t

val one : Conj.t -> t
(** Alias for {!singleton}. *)

val two : Conj.t -> Conj.t -> t

val and_ : t -> t -> t
val and_l : t list -> t

val or_ : t -> t -> t
val or_l : t list -> t

val quantify : Var.t -> t -> t

val to_seq : t -> Conj.t Seq.t
val to_list : t -> Conj.t list

val map : (Conj.t -> Conj.t) -> t -> t
val concat_map : (Conj.t -> t) -> t -> t

val from_list : Conj.t list -> t
