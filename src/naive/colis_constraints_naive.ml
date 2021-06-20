open Colis_constraints_common

module Assign = Assign
module Engine = Engine
module Metavar = Metavar
module Pattern = Pattern
module Rules = Rules

let quantify_over x conj =
  let x' = Var.fresh () in
  conj
  |> XConstraint.quantify x'
  |> XConstraint.set_literals
    (XConstraint.literals conj
     |> Rules.replace_in_literals ~var:x ~by:x')
  |> DXC.singleton
  |> Engine.normalize

let quantify_over_and_simplify x conj =
  quantify_over x conj
  |> Engine.simplify

let with_shadow_variables f = f ()

let normalise = Engine.normalize
let simplify = Engine.simplify
