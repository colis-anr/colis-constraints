type t = XConstraint.t list
[@@deriving show { with_path = false }]
(* FIXME: abstract *)

let pp fmt = function
  | [] -> Format.pp_print_string fmt "⊥"
  | [conj] -> XConstraint.pp fmt conj
  | disj -> pp fmt disj (* FIXME *)

let false_ = []
let empty = false_
let singleton conj = [conj]
let one = singleton
let two c1 c2 = [c1; c2]

let and_ d1 d2 =
  List.map (fun c1 -> List.map (XConstraint.and_ c1) d2) d1
  |> List.flatten

let and_l = function
  | [] -> invalid_arg "and_l"
  | d :: ds -> List.fold_left and_ d ds

let or_ = (@)
let or_l = List.flatten

let quantify x d = List.map (XConstraint.quantify x) d

let to_seq = List.to_seq
let to_list = fun x -> x

let map = List.map
let concat_map = ExtList.concat_map

let from_list = fun x -> x
