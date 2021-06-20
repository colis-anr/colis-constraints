open Colis_constraints_common

module Assign = Assign
module Engine = Engine
module Metavar = Metavar
module Pattern = Pattern
module Rules = Rules

val quantify_over : Var.t -> XConstraint.t -> DXC.t
val quantify_over_and_simplify : Var.t -> XConstraint.t -> DXC.t

val normalise : ?limit:int -> DXC.t -> DXC.t
val simplify : DXC.t -> DXC.t

val with_shadow_variables : (unit -> 'a) -> 'a
(** Undocumented *)
