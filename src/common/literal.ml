type t =
  | Pos of Atom.t
  | Neg of Atom.t
[@@deriving eq, ord]

let pp fmt = function
  | Pos a -> Atom.pp fmt a
  | Neg a -> Format.fprintf fmt "¬ %a" Atom.pp a

let vars = function
  | Pos a | Neg a ->
     Atom.vars a

let rewrite_variables s = function
  | Pos a -> Pos (Atom.rewrite_variables s a)
  | Neg a -> Neg (Atom.rewrite_variables s a)

module Self = struct
  type s = t
  type t = s

  let compare = compare
  let equal = equal
end

module Set = BatSet.Make(Self)
