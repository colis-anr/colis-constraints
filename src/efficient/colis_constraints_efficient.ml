open Colis_constraints_common
include External

type sat_conj = Core.t                                       [@@deriving yojson]
let true_sat_conj = Core.empty

let pp_sat_conj = PrettyPrinter.pp
let pp_sat_conj_as_dot = PrettyPrinter.pp_as_dot

let simplify = Core.simplify

let quantify_over = Core.quantify_over

let quantify_over_and_simplify x c =
  Core.(c |> quantify_over x |> simplify)

let with_shadow_variables = Core.with_shadow_variables

let add_to_sat_conj constr sat_conj =
  constr
  |> to_disj
  |> DXC.to_list
  |> ExtList.concat_map
    (fun conj ->
       XConstraint.literals conj
       |> Seq.fold_left
         (fun sat_conj_list lit ->
            ExtList.concat_map (External.literal lit) sat_conj_list)
         [sat_conj])

let sat_conj_to_literals = Core.to_literals
