open Batteries
open Colis_constraints_common

(** {2 Base Types} *)

type var = Var.t
type feat = Feat.t
type feat_set = Feat.Set.t

(** {2 Constraints} *)

type t
(** Type of constraints. *)

val true_ : t
(** The true constraint. *)

val eq : var -> var -> t
val neq : var -> var -> t

val feat : var -> feat -> var -> t
val nfeat : var -> feat -> var -> t

val abs : var -> feat -> t
val nabs : var -> feat -> t

val maybe : var -> feat -> var -> t
val nmaybe : var -> feat -> var -> t

val fen : var -> feat_set -> t
val nfen : var -> feat_set -> t

val sim : var -> feat_set -> var -> t
val nsim : var -> feat_set -> var -> t
val sim1 : var -> feat -> var -> t

val reg : var -> t
val nreg : var -> t
val dir : var -> t
val ndir : var -> t
val block : var -> t
val nblock : var -> t
val sock : var -> t
val nsock : var -> t
val pipe : var -> t
val npipe : var -> t
val char : var -> t
val nchar : var -> t
val symlink : var -> t
val nsymlink : var -> t

val resolve : var -> Path.normal -> Path.t -> var -> t
val noresolve : var -> Path.normal -> Path.t -> t
val maybe_resolve : var -> Path.normal -> Path.t -> var -> t
val similar : var -> var -> Path.normal -> Path.t -> var -> var -> t

val literal : Literal.t -> t

val with_shadow_variables : (unit -> 'a) -> 'a

val exists  : (var -> t) -> t
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

val sat_conj_to_literals : sat_conj -> Literal.t Seq.t
