open Batteries

type t

val true_ : t

val quantify : Var.t -> t -> t
val unquantify : Var.t -> t -> t
val set_quantified_variables : Var.t Seq.t -> t -> t
(** Conjunction with the same literals but the new quantification block. *)

val add_literal : Literal.t -> t -> t
val add_literals_list : Literal.t list -> t -> t
val set_literals : Literal.t Seq.t -> t -> t
(** Conjunction with the same quantification block but the new literals. *)

val and_ : t -> t -> t

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : Format.formatter -> t -> unit
val pp_as_dot : name:string -> Format.formatter -> t -> unit

val quantified_variables : t -> Var.t Seq.t
val quantified_variables_set : t -> Var.Set.t

val literals : t -> Literal.t Seq.t
val literals_set : t -> Literal.Set.t
val literals_list : t -> Literal.t list

val from_sets : Var.Set.t -> Literal.Set.t -> t

val is_quantified : Var.t -> t -> bool
