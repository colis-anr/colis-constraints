let with_internal x f c =
  let (x, c) = Core.internalise x c in
  f x c

let with_internal_2 x y f =
  with_internal x @@ fun x ->
  with_internal y @@ fun y ->
  f x y

let literal lit =
  let open Colis_constraints_common in
  let open Atom in let open Literal in
  Colis_constraints_common.Limits.check_cpu_time_limit ();
  Colis_constraints_common.Limits.check_memory_limit ();
  match lit with
  | Pos (Eq (x, y)) ->
    with_internal_2 x y @@ fun x y ->
    Internal.eq x y

  | Pos (Feat (x, f, y)) ->
    with_internal_2 x y @@ fun x y ->
    Internal.feat x f y

  | Pos (Abs (x, f)) ->
    with_internal x @@ fun x ->
    Internal.abs x f

  | Pos (Maybe (x, f, y)) ->
    with_internal_2 x y @@ fun x y ->
    Internal.maybe x f y

  | Pos (Kind (x, k)) ->
    with_internal x @@ fun x ->
    Internal.kind x k

  | Pos (Fen (x, fs)) ->
    with_internal x @@ fun x ->
    Internal.fen x fs

  | Pos (Sim (x, fs, y)) ->
    with_internal_2 x y @@ fun x y ->
    Internal.sim x fs y

  | Neg (Eq (x, y)) ->
    with_internal x @@ fun x ->
    Internal.neq x y

  | Neg (Feat (x, f, y)) ->
    with_internal x @@ fun x ->
    Internal.nfeat x f y

  | Neg (Abs (x, f)) ->
    with_internal x @@ fun x ->
    Internal.nabs x f

  | Neg (Maybe (x, f, y)) ->
    with_internal_2 x y @@ fun x y ->
    Internal.nmaybe x f y

  | Neg (Kind (x, k)) ->
    with_internal x @@ fun x ->
    Internal.nkind x k

  | Neg (Fen (_x, _fs)) ->
    Core.not_implemented "nfen"

  | Neg (Sim (_x, _fs, _y)) ->
    Core.not_implemented "nsim"
