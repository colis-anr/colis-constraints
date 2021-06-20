open Colis_constraints_common

module Assign = Assign
module Engine = Engine
module Metavar = Metavar
module Pattern = Pattern
module Rules = Rules

val quantify_over : Var.t -> Conj.t -> Disj.t
val quantify_over_and_simplify : Var.t -> Conj.t -> Disj.t

val normalise : ?limit:int -> Disj.t -> Disj.t
val simplify : Disj.t -> Disj.t

val with_shadow_variables : (unit -> 'a) -> 'a
(** Undocumented *)
