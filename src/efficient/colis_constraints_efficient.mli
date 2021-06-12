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

(** {3 Basic Constraints} *)

val eq : var -> var -> t
(** The base constraint representing the equality between two variables. *)

val neq : var -> var -> t
(** Same as {!eq} for disequality. *)

val feat : var -> feat -> var -> t
(** Same as {!eq} for feature constraint. *)

val nfeat : var -> feat -> var -> t
(** Same as {!eq} for negated feature constraint. *)

val abs : var -> feat -> t
(** Same as {!eq} for absence constraint. *)

val nabs : var -> feat -> t
(** Same as {!eq} for negated absence constraint. *)

val maybe : var -> feat -> var -> t
(** Same as {!eq} for maybe constraint. [maybe x f y] is equivalent to [abs x f
    ∨ feat x f y] but the representation is more efficient. *)

val nmaybe : var -> feat -> var -> t
(** Same as {!eq} for negated maybe constraints. *)

val fen : var -> feat_set -> t
(** Same as {!eq} for fence constraints. *)

val nfen : var -> feat_set -> t
(** Same as {!eq} for negated fence constraints. *)

val sim : var -> feat_set -> var -> t
(** Same as {!eq} for similarity constraints. *)

val nsim : var -> feat_set -> var -> t
(** Same as {!eq} for negated similarity constraints. *)

val sim1 : var -> feat -> var -> t
(** Same as {!sim} for a singleton set. *)

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
(** Same as {!eq} for the various file kinds constraints, negated or not. *)

val literal : Literal.t -> t
(** Build a constraint from a common literal. *)

(** {3 Macros} *)

val resolve : var -> Path.normal -> Path.t -> var -> t
val noresolve : var -> Path.normal -> Path.t -> t
val maybe_resolve : var -> Path.normal -> Path.t -> var -> t
val similar : var -> var -> Path.normal -> Path.t -> var -> var -> t

(** {3 Higher-order Constraints} *)

val and_ : t -> t -> t
val (&) : t -> t -> t
(** Conjunction of two constraints, prefix and infix versions. *)

val and_l : t list -> t
(** Conjunction of a list of constraints. *)

val or_ : t -> t -> t
val or_l : t list -> t
(** Disjunction of constraints. The disjunctions are always put in disjunctive
   normal form, which makes their handling highly inefficient; beware! *)

val exists  : (var -> t) -> t
(** Existential quantification of constraints. {!exists} takes a function as
   argument and gives it a fresh variable as argument. The function can use the
   fresh variable to build a constraint. The variable is then quantified upon. *)

val exists2 : (var -> var -> t) -> t
val exists3 : (var -> var -> var -> t) -> t
val exists4 : (var -> var -> var -> var -> t) -> t
val exists5 : (var -> var -> var -> var -> var -> t) -> t
val exists6 : (var -> var -> var -> var -> var -> var -> t) -> t
val exists7 : (var -> var -> var -> var -> var -> var -> var -> t) -> t
val exists8 : (var -> var -> var -> var -> var -> var -> var -> var -> t) -> t
val exists9 : (var -> var -> var -> var -> var -> var -> var -> var -> var -> t) -> t
(** Same as {!exists} for several variables at the same time. *)

(** {2 Satisfiable Conjunctions} *)

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
val quantify_over : var -> sat_conj -> sat_conj
val quantify_over_and_simplify : var -> sat_conj -> sat_conj

val pp_sat_conj : Format.formatter -> sat_conj -> unit
val pp_sat_conj_as_dot : name:string -> Format.formatter -> sat_conj -> unit

val sat_conj_to_literals : sat_conj -> Literal.t Seq.t

(** {2 Undocumented} *)

val with_shadow_variables : (unit -> 'a) -> 'a
