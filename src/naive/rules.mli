open Colis_constraints_common

val accessibility : Literal.Set.t -> (Var.t * Var.Set.t) list
val replace_in_literals : var:Var.t -> by:Var.t -> Literal.t Seq.t -> Literal.t Seq.t

val all : (string * (XConstraint.t -> DXC.t option)) list
