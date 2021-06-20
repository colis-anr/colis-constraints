(** {1 Colis Constraints -- Common} *)

type t
(** Type of constraints. *)

(** {2 Building Constraints} *)

(** {3 Basic Constraints} *)

val true_ : t
(** The true constraint. *)

val eq : Var.t -> Var.t -> t
(** The base constraint representing the equality between two variables. *)

val neq : Var.t -> Var.t -> t
(** Same as {!eq} for disequality. *)

val feat : Var.t -> Feat.t -> Var.t -> t
(** Same as {!eq} for feature constraint. *)

val nfeat : Var.t -> Feat.t -> Var.t -> t
(** Same as {!eq} for negated feature constraint. *)

val abs : Var.t -> Feat.t -> t
(** Same as {!eq} for absence constraint. *)

val nabs : Var.t -> Feat.t -> t
(** Same as {!eq} for negated absence constraint. *)

val maybe : Var.t -> Feat.t -> Var.t -> t
(** Same as {!eq} for maybe constraint. [maybe x f y] is equivalent to [abs x f
    ∨ Feat.t x f y] but the representation is more efficient. *)

val nmaybe : Var.t -> Feat.t -> Var.t -> t
(** Same as {!eq} for negated maybe constraints. *)

val fen : Var.t -> Feat.Set.t -> t
(** Same as {!eq} for fence constraints. *)

val nfen : Var.t -> Feat.Set.t -> t
(** Same as {!eq} for negated fence constraints. *)

val sim : Var.t -> Feat.Set.t -> Var.t -> t
(** Same as {!eq} for similarity constraints. *)

val nsim : Var.t -> Feat.Set.t -> Var.t -> t
(** Same as {!eq} for negated similarity constraints. *)

val reg : Var.t -> t
val nreg : Var.t -> t
val dir : Var.t -> t
val ndir : Var.t -> t
val block : Var.t -> t
val nblock : Var.t -> t
val sock : Var.t -> t
val nsock : Var.t -> t
val pipe : Var.t -> t
val npipe : Var.t -> t
val char : Var.t -> t
val nchar : Var.t -> t
val symlink : Var.t -> t
val nsymlink : Var.t -> t
(** Same as {!eq} for the Var.tious file kinds constraints, negated or not. *)

val literal : Literal.t -> t
(** Build a constraint from a common literal. *)

(** {3 Macros} *)

val resolve : Var.t -> Path.normal -> Path.t -> Var.t -> t
val noresolve : Var.t -> Path.normal -> Path.t -> t
val maybe_resolve : Var.t -> Path.normal -> Path.t -> Var.t -> t
val similar : Var.t -> Var.t -> Path.normal -> Path.t -> Var.t -> Var.t -> t

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

val exists  : (Var.t -> t) -> t
(** Existential quantification of constraints. {!exists} takes a function as
   argument and gives it a fresh variable as argument. The function can use the
   fresh variable to build a constraint. The variable is then quantified upon. *)

val exists2 : (Var.t -> Var.t -> t) -> t
val exists3 : (Var.t -> Var.t -> Var.t -> t) -> t
val exists4 : (Var.t -> Var.t -> Var.t -> Var.t -> t) -> t
val exists5 : (Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> t) -> t
val exists6 : (Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> t) -> t
val exists7 : (Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> t) -> t
val exists8 : (Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> t) -> t
val exists9 : (Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> Var.t -> t) -> t
(** Same as {!exists} for several variables at the same time. *)

(** {2 Working with Constraints} *)

val to_disj : t -> DXC.t
(** For internal use only. *)

(** {2 Submodules} *)

(** {3 Basic Constraint Objects} *)

module Feat = Feat
module Kind = Kind
module Var = Var

(** {3 Formulas}

    Various classes of formulas. The various names are taken from Nicolas
    Jeannerod's Ph.D. Thesis: “Verification of Shell Scripts Performing File
    Hierarchy Transformations”. *)

module Atom = Atom
(** Atomic formulas, aka predicates. *)

module Literal = Literal
(** Literals: atom and negated atoms. *)

module XConstraint = XConstraint
(** X-Constraints: existentially quantified conjunctions of literals. *)

module DXC = DXC
(** Disjunctions of x-constraints. *)

(** {3 Misc} *)

module Log = Log
module Path = Path

module Derivable = Derivable

module Limits = Limits

module ExtList = ExtList
