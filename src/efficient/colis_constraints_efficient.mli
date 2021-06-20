open Batteries
open Colis_constraints_common

type sat_conj
(** Type of satisfiable conjunctions. The interface guarantees that all objects
    of this type are satisfiable. *)

val true_sat_conj : sat_conj
(** The empty satisfiable conjunction, trivially true. *)

val add_to_sat_conj : t -> sat_conj -> sat_conj Dnf.t
(** Add a constraint of type {!t} to a satisfiable conjunction. This yields a
    DNF of satisfiable conjunction. The DNF can of course be empty if the added
    constraints make the whole formula unsatisfiable. *)

val simplify : sat_conj -> sat_conj
val quantify_over : Var.t -> sat_conj -> sat_conj
val quantify_over_and_simplify : Var.t -> sat_conj -> sat_conj

val pp_sat_conj : Format.formatter -> sat_conj -> unit
val pp_sat_conj_as_dot : name:string -> Format.formatter -> sat_conj -> unit

val sat_conj_to_literals : sat_conj -> Literal.t Seq.t

val with_shadow_variables : (unit -> 'a) -> 'a
(** Undocumented. *)
