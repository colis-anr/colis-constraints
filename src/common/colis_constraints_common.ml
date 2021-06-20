(** {1 Colis Constraints -- Common} *)

(** {2 Constraints} *)

type t = DXC.t

let to_disj = fun x -> x

let true_ = DXC.singleton XConstraint.true_
let false_ = DXC.false_

let and_ = DXC.and_
let (&) = and_
let and_l = DXC.and_l

let or_ = DXC.or_
let or_l = DXC.or_l

let literal lit =
  DXC.singleton XConstraint.(add_literal lit true_)

let eq x y = literal (Literal.Pos (Atom.Eq (x, y)))
let neq x y = literal (Literal.Neg (Atom.Eq (x, y)))
let feat x f y = literal (Literal.Pos (Atom.Feat (x, f, y)))
let nfeat x f y = literal (Literal.Neg (Atom.Feat (x, f, y)))
let abs x f = literal (Literal.Pos (Atom.Abs (x, f)))
let nabs x f = literal (Literal.Neg (Atom.Abs (x, f)))
let maybe x f y = literal (Literal.Pos (Atom.Maybe (x, f, y)))
let nmaybe x f y = literal (Literal.Neg (Atom.Maybe (x, f, y)))
let kind x k = literal (Literal.Pos (Atom.Kind (x, k)))
let nkind x k = literal (Literal.Neg (Atom.Kind (x, k)))
let fen x fs = literal (Literal.Pos (Atom.Fen (x, fs)))
let nfen x fs = literal (Literal.Neg (Atom.Fen (x, fs)))
let sim x fs y = literal (Literal.Pos (Atom.Sim (x, fs, y)))
let nsim x fs y = literal (Literal.Neg (Atom.Sim (x, fs, y)))

let reg x = kind x Kind.Reg
let nreg x = nkind x Kind.Reg
let dir x = kind x Kind.Dir
let ndir x = nkind x Kind.Dir
let block x = kind x Kind.Block
let nblock x = nkind x Kind.Block
let sock x = kind x Kind.Sock
let nsock x = nkind x Kind.Sock
let pipe x = kind x Kind.Pipe
let npipe x = nkind x Kind.Pipe
let char x = kind x Kind.Char
let nchar x = nkind x Kind.Char
let symlink x = kind x Kind.Symlink
let nsymlink x = nkind x Kind.Symlink

let exists f =
  let x = Var.fresh () in
  let d = f x in
  DXC.quantify x d

let exists2 f = exists  @@ fun x1 -> exists @@ fun x2 -> f x1 x2
let exists3 f = exists2 @@ fun x1 x2 -> exists @@ fun x3 -> f x1 x2 x3
let exists4 f = exists3 @@ fun x1 x2 x3 -> exists @@ fun x4 -> f x1 x2 x3 x4
let exists5 f = exists4 @@ fun x1 x2 x3 x4 -> exists @@ fun x5 -> f x1 x2 x3 x4 x5
let exists6 f = exists5 @@ fun x1 x2 x3 x4 x5 -> exists @@ fun x6 -> f x1 x2 x3 x4 x5 x6
let exists7 f = exists6 @@ fun x1 x2 x3 x4 x5 x6 -> exists @@ fun x7 -> f x1 x2 x3 x4 x5 x6 x7
let exists8 f = exists7 @@ fun x1 x2 x3 x4 x5 x6 x7 -> exists @@ fun x8 -> f x1 x2 x3 x4 x5 x6 x7 x8
let exists9 f = exists8 @@ fun x1 x2 x3 x4 x5 x6 x7 x8 -> exists @@ fun x9 -> f x1 x2 x3 x4 x5 x6 x7 x8 x9

let rec resolve x pi q z =
  match Path.split_first_rel q with
  | None -> eq x z
  | Some (Down f, q) ->
    exists @@ fun y ->
    feat x f y & resolve y (x :: pi) q z
  | Some (Here, q) ->
    resolve x pi q z
  | Some (Up, q) ->
    match pi with
    | [] -> resolve x [] q z
    | y::pi -> dir x & resolve y pi q z

let resolve r cwd p z =
  resolve r [] (Path.concat cwd p) z

let rec noresolve x pi q =
  (* FIXME: redefine using `maybe_resolve`? *)
  match Path.split_first_rel q with
  | None -> false_
  | Some (Down f, q) ->
    (match Path.split_first_rel q with
     | None ->
       abs x f
     | _ ->
       exists @@ fun y ->
       maybe x f y & noresolve y (x::pi) q)
  | Some (Here, q) ->
    noresolve x pi q
  | Some (Up, q) ->
    match pi with
    | [] -> noresolve x [] q
    | y::pi -> or_ (nkind x Kind.Dir) (noresolve y pi q)

let noresolve r cwd p =
  noresolve r [] (Path.concat cwd p)

let rec maybe_resolve x pi q z =
  match Path.split_first_rel q with
  | None -> eq x z
  | Some (Down f, q) ->
    exists @@ fun y ->
    maybe x f y & maybe_resolve y (x::pi) q z
  | Some (Here, q) ->
    maybe_resolve x pi q z
  | Some (Up, q) ->
    match pi with
    | [] -> maybe_resolve x [] q z
    | y::pi -> or_ (nkind x Kind.Dir) (maybe_resolve y pi q z)

let maybe_resolve r cwd p z =
  maybe_resolve r [] (Path.concat cwd p) z

let rec similar x x' p z z' =
  match p with
  | [] ->
    eq x z & eq x' z'
  | f::p ->
    exists @@ fun y -> exists @@ fun y' ->
    feat x f y & feat x' f y' & sim x (Feat.Set.singleton f) x' & similar y y' p z z'

let similar r r' cwd q z z' =
  similar r r' (Path.normalize ~cwd q) z z'

(** {2 Submodules} *)

module Feat = Feat
module Kind = Kind
module Var = Var

module Atom = Atom
module Literal = Literal
module XConstraint = XConstraint
module DXC = DXC

module Log = Log
module Path = Path

module Derivable = Derivable

module Limits = Limits

module ExtList = ExtList
