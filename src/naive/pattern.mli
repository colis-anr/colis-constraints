open Batteries
open Colis_constraints_common

type atom =
  | Eq of Metavar.t * Metavar.t
  | Feat of Metavar.t * Metavar.t * Metavar.t
  | Abs of Metavar.t * Metavar.t
  | Kind of Metavar.t * Metavar.t
  | Fen of Metavar.t * Metavar.t
  | Sim of Metavar.t * Metavar.t * Metavar.t

type literal =
  | Pos of atom
  | Neg of atom

val find_all :
  ?pred:(Assign.t -> XConstraint.t -> bool) ->
  literal list -> XConstraint.t ->
  (Assign.t * XConstraint.t) Seq.t
