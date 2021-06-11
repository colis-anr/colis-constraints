open Colis_constraints_efficient_clause
include module type of External

(** {2 Base Types} *)

type var
type feat

(** {2 Constraints} *)

type t
(** Type of constraints. *)

val true_ : t
(** The true constraint. *)

val sim1 : var -> feat -> var -> t
(** Same as {!sim} but for a singleton. *)

val with_shadow_variables : (unit -> 'a) -> 'a

val exists : (var -> t) -> t
val exists2 : (var -> var -> t) -> t
val exists3 : (var -> var -> var -> t) -> t
val exists4 : (var -> var -> var -> var -> t) -> t
val exists5 : (var -> var -> var -> var -> var -> t) -> t
val exists6 : (var -> var -> var -> var -> var -> var -> t) -> t
val exists7 : (var -> var -> var -> var -> var -> var -> var -> t) -> t
val exists8 : (var -> var -> var -> var -> var -> var -> var -> var -> t) -> t
val exists9 : (var -> var -> var -> var -> var -> var -> var -> var -> var -> t) -> t

val and_ : t -> t -> t
val (&) : t -> t -> t
(** Conjunction of two raw literals, prefix and infix versions. *)

val and_l : t list -> t
(** Conjunction of a list of raw literals. *)

val or_ : t -> t -> t
val or_l : t list -> t
(** Disjunction of raw literals. (Warning: inefficient.) *)

(** {2 Satisfiable Conjunctions} *)

type sat_conj
(** Type of satisfiable conjunctions. The interface guarantees that all objects
   of this type are satisfiable. *)

val true_sat_conj : sat_conj

val add_to_sat_conj : t -> sat_conj -> sat_conj Dnf.t
(** Add a constraint of type {!t} to a satisfiable conjunction. This yields a
   DNF of satisfiable conjunction. The DNF can of course be empty if the added
   constraints make the whole formula unsatisfiable. *)

val simplify : sat_conj -> sat_conj
val quantify_over : var -> sat_conj -> sat_conj
val quantify_over_and_simplify : var -> sat_conj -> sat_conj

val pp_sat_conj : Format.formatter -> sat_conj -> unit
val pp_sat_conj_as_dot : name:string -> Format.formatter -> sat_conj -> unit

val sat_conj_to_literals : sat_conj -> Colis_constraints_common.Literal.t Seq.t
