open Batteries
open Colis_constraints_common open Atom open Literal

let accessibility c =
  (* Create the graph and fill it. *)
  let reach = Hashtbl.create 8 in
  let edges = Hashtbl.create 8 in
  Literal.Set.iter
    (function
     | Pos (Feat (x, _, y)) ->
        Hashtbl.replace reach y ();
        Hashtbl.add edges x y
     | _ -> ())
    c;
  (* Create a queue and add the nodes that are reachable from
     nobody. *)
  let queue = Queue.create () in
  Hashtbl.iter
    (fun x _ ->
      if not (Hashtbl.mem reach x) then
        Queue.push (x, Var.Set.empty) queue)
    edges;
  let res = Hashtbl.create 8 in
  let rec aux () =
    if not (Queue.is_empty queue) then
      (
        (* Get a node [x] and a bunch of nodes [y]s
           that lead to [x]. *)
        let (x, ys) = Queue.pop queue in
        (* We check for cycles. *)
        if Var.Set.mem x ys then
          raise (Invalid_argument "accessibility");
        (* We add the [y]s to the result: they lead to [x]. *)
        let ys =
          match Hashtbl.find_option res x with
          | None -> ys
          | Some zs -> Var.Set.union ys zs
        in
        Hashtbl.replace res x ys;
        (* For each node [z] accessible from [x], we carry on. *)
        let ys = Var.Set.add x ys in
        Hashtbl.find_all edges x
        |> List.iter (fun z -> Queue.add (z, ys) queue);
        aux ()
      )
  in
  aux ();
  Hashtbl.fold (fun x ys l -> (x, ys) :: l) res []

let replace_in_atom ~var:x ~by:y = function
  | Eq (a, b) -> Eq ((if Var.equal a x then y else a), (if Var.equal b x then y else b))
  | Feat (a, f, b) -> Feat ((if Var.equal a x then y else a), f, (if Var.equal b x then y else b))
  | Abs (a, f) -> Abs ((if Var.equal a x then y else a), f)
  | Maybe (a, f, b) -> Maybe ((if Var.equal a x then y else a), f, (if Var.equal b x then y else b))
  | Kind (a, k) -> Kind ((if Var.equal a x then y else a), k)
  | Fen (a, fs) -> Fen ((if Var.equal a x then y else a), fs)
  | Sim (a, fs, b) -> Sim ((if Var.equal a x then y else a), fs, (if Var.equal b x then y else b))

let replace_in_literal ~var ~by = function
  | Pos a -> Pos (replace_in_atom ~var ~by a)
  | Neg a -> Neg (replace_in_atom ~var ~by a)

let replace_in_literals ~var ~by =
  Seq.map (replace_in_literal ~var ~by)

let (&) = Seq.cons

let (x, y, z) = Metavar.fresh3 ()
let f = Metavar.fresh ()
let (fs, gs) = Metavar.fresh2 ()
let (k, l) = Metavar.fresh2 ()

let c_cycle conj =
  try ignore (accessibility (XConstraint.literals_set conj)); None
  with Invalid_argument _ -> Some DXC.false_

let make ~pat ?pred ~prod () =
  fun conj ->
  (* let es = XConstraint.quantified_variables_set conj in
   * let conj = XConstraint.literals conj in *)
  Pattern.find_all ?pred pat conj
  |> Seq.filter_map
    (fun (a, conj') ->
       (* The production rule gives us a disjunction. However,
          sometimes, this conjunction might be equal to the given
          one. In this case, we have to keep looking for an other
          production rule that might be better. *)
       match DXC.to_list (prod a conj') with
       | [] -> Some DXC.false_
       | [conj'] when XConstraint.equal conj' conj -> None
       | [conj'] -> Some (DXC.singleton conj')
       | disj' ->
         assert (List.for_all (fun conj' -> not (XConstraint.equal conj' conj)) disj');
         Some (DXC.from_list disj'))
  |> (fun seq -> seq ())
  |> function
  | Nil -> None
  | Cons (x, _) -> Some x

let clash _ _ = DXC.false_

let c_feat_abs =
  make
    ~pat:[Pos (Feat (x, f, y)); Pos (Abs (x, f))]
    ~prod:clash
    ()

let c_feat_fen =
  make
    ~pat:[Pos (Feat (x, f, y)); Pos (Fen (x, fs))]
    ~pred:(fun a _ -> not (Feat.Set.mem (Assign.feat a f) (Assign.feat_set a fs)))
    ~prod:clash
    ()

let c_neq_refl =
  make
    ~pat:[Neg (Eq (x, x))]
    ~prod:clash
    ()

let c_nsim_refl =
  make
    ~pat:[Neg (Sim (x, fs, x))]
    ~prod:clash
    ()

let c_kinds =
  make
    ~pat:[Pos (Kind (x, k)); Pos (Kind (x, l))]
    ~pred:(fun a _ -> Assign.kind a k <> Assign.kind a l)
    ~prod:clash
    ()

let s_eq_glob =
  (* Note: this rule is not in the article but is necessary here. This is
     because we need to check for satifiability of formulas, which is not what
     is stated in the article. *)
  make
    ~pat:[Pos (Eq (x, y))]
    ~pred:(fun a _ ->
        not (Var.equal (Assign.var a x) (Assign.var a y)))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let (x, y) =
          (* Replace the newest one by the oldest one. *)
          match Var.compare x y with
          | c when c < 0 -> (y, x)
          | c when c > 0 -> (x, y)
          | _ -> assert false
        in
        conj
        |> XConstraint.set_literals
          (Pos (Eq (x, y)) & replace_in_literals ~var:x ~by:y (XConstraint.literals conj))
        |> DXC.singleton)
    ()

let s_eq =
  make
    ~pat:[Pos (Eq (x, y))]
    ~pred:(fun a conj ->
        Var.Set.mem (Assign.var a x) (XConstraint.quantified_variables_set conj)
        && not (Var.equal (Assign.var a x) (Assign.var a y)))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        conj
        |> XConstraint.unquantify x
        |> XConstraint.set_literals
          (conj
           |> XConstraint.literals
           |> replace_in_literals ~var:x ~by:y)
        |> DXC.singleton)
    ()

let s_eq_refl =
  make
    ~pat:[Pos (Eq (x, x))]
    ~prod:(fun _ conj -> DXC.singleton conj)
    ()

let s_feats =
  make
    ~pat:[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))]
    ~pred:(fun a conj ->
        XConstraint.is_quantified (Assign.var a z) conj
        && not (Var.equal (Assign.var a y) (Assign.var a z)))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let z = Assign.var a z in
        let f = Assign.feat a f in
        conj
        |> XConstraint.unquantify z
        |> XConstraint.set_literals
          (conj
           |> XConstraint.literals
           |> replace_in_literals ~var:z ~by:y
           |> (&) (Pos (Feat (x, f, y))))
        |> DXC.singleton)
    ()

let s_feats_glob =
  make
    ~pat:[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))]
    ~pred:(fun a conj ->
      not (XConstraint.is_quantified (Assign.var a y) conj)
      && not (XConstraint.is_quantified (Assign.var a z) conj))
    ~prod:(fun a conj ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let z = Assign.var a z in
      let f = Assign.feat a f in
      conj
      |> XConstraint.add_literals_list
        [ Pos (Eq (y, z)) ;
          Pos (Feat (x, f, y)) ]
      |> DXC.singleton)
    ()

let s_sims =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Sim (x, gs, y))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let hs = Feat.Set.inter (Assign.feat_set a fs) (Assign.feat_set a gs) in
        conj
        |> XConstraint.add_literal (Pos (Sim (x, hs, y)))
        |> DXC.singleton)
    ()

let p_feat =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Feat (x, f, z))]
    ~pred:(fun a _ -> not (Feat.Set.mem (Assign.feat a f) (Assign.feat_set a fs)))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let z = Assign.var a z in
        let f = Assign.feat a f in
        let fs = Assign.feat_set a fs in
        conj
        |> XConstraint.add_literals_list
          [Pos (Sim (x, fs, y));
           Pos (Feat (x, f, z));
           Pos (Feat (y, f, z))]
        |> DXC.singleton)
    ()

let p_abs =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Abs (x, f))]
    ~pred:(fun a _ -> not (Feat.Set.mem (Assign.feat a f) (Assign.feat_set a fs)))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let f = Assign.feat a f in
        let fs = Assign.feat_set a fs in
        conj
        |> XConstraint.add_literals_list
          [Pos (Sim (x, fs, y));
           Pos (Abs (x, f));
           Pos (Abs (y, f))]
        |> DXC.singleton)
    ()

let p_fen =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Fen (x, gs))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        conj
        |> XConstraint.add_literals_list
          [Pos (Sim (x, fs, y));
           Pos (Fen (x, gs));
           Pos (Fen (y, Feat.Set.union fs gs))]
        |> DXC.singleton)
    ()

let p_sim =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Sim (x, gs, z))]
    ~pred:(fun a conj ->
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        let fgs = Feat.Set.union fs gs in
        (* This rule has a really heavy side-condition *)
        let hs =
          Seq.fold_left
            (fun hs l ->
               match l with
               | Pos (Sim (_, hs', _)) ->
                 Some (
                   match hs with
                   | None -> hs'
                   | Some hs -> Feat.Set.inter hs hs'
                 )
               | _ -> hs)
            None
            (XConstraint.literals conj)
        in
        match hs with
        | None -> true
        | Some hs -> not (Feat.Set.subset hs fgs))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let z = Assign.var a z in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        let fgs = Feat.Set.union fs gs in
        conj
        |> XConstraint.add_literals_list
          [ Pos (Sim (x, fs, y));
            Pos (Sim (x, gs, z));
            Pos (Sim (y, fgs, z)) ]
        |> DXC.singleton)
    ()

let r_neq =
  make
    ~pat:[Neg (Eq (x, y))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        conj
        |> XConstraint.add_literal
          (Neg (Sim (x, Feat.Set.empty, y)))
        |> DXC.singleton)
    ()

let r_nfeat =
  make
    ~pat:[Neg (Feat (x, f, y))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let f = Assign.feat a f in
        DXC.two
          (XConstraint.add_literal (Pos (Abs (x, f))) conj)
          (let z = Var.fresh () in
           conj
           |> XConstraint.quantify z
           |> XConstraint.add_literals_list
             [ Pos (Feat (x, f, z));
               Neg (Sim (y, Feat.Set.empty, z)) ])
      )
    ()

let r_nkind =
  make
    ~pat:[Neg (Kind (x, k))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let k = Assign.kind a k in
        Kind.all
        |> List.filter ((<>) k)
        |> List.map
          (fun k ->
             XConstraint.add_literal (Pos (Kind (x, k))) conj)
        |> DXC.from_list
      )
    ()

let r_nabs =
  make
    ~pat:[Neg (Abs (x, f))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let f = Assign.feat a f in
        let z = Var.fresh () in
        conj
        |> XConstraint.quantify z
        |> XConstraint.add_literal (Pos (Feat (x, f, z)))
        |> DXC.singleton)
    ()

let one_feature_in x fs conj =
  Feat.Set.elements fs
  |> List.map
    (fun f ->
       let z = Var.fresh () in
       conj
       |> XConstraint.quantify z
       |> XConstraint.add_literal (Pos (Feat (x, f, z))))
  |> DXC.from_list

let one_difference_in x y fs conj =
  let difference_in x y f es c =
    let z = Var.fresh () in
    let z' = Var.fresh () in
    [(Var.Set.add z' es, Pos (Abs (x, f)) & Pos (Feat (y, f, z')) & c) ;
     (Var.Set.add z es, Pos (Feat (x, f, z)) & Pos (Abs (y, f)) & c) ;
     (Var.Set.add z (Var.Set.add z' es), Pos (Feat (x, f, z)) & Pos (Feat (y, f, z')) & Neg (Sim (z, Feat.Set.empty, z')) & c)]
  in
  Feat.Set.elements fs
  |> List.map
    (fun f ->
       difference_in x y f
         (XConstraint.quantified_variables_set conj)
         (XConstraint.literals conj))
  |> List.flatten
  |> List.map (fun (es, c) -> XConstraint.from_sets es (Literal.Set.of_seq c))
  |> DXC.from_list

let r_nfen_fen =
  make
    ~pat:[Pos (Fen (x, fs)); Neg (Fen (x, gs))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        one_feature_in
          x (Feat.Set.diff fs gs)
          (XConstraint.add_literal (Pos (Fen (x, fs))) conj))
    ()

let r_nsim_sim =
  make
    ~pat:[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, y))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        one_difference_in
          x y (Feat.Set.diff fs gs)
          (XConstraint.add_literal (Pos (Sim (x, fs, y))) conj))
    ()

let r_nsim_fen =
  make
    ~pat:[Pos (Fen (x, fs)); Neg (Sim (x, gs, y))]
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        let conj = XConstraint.add_literal (Pos (Fen (x, fs))) conj in
        DXC.or_
          (conj
           |> XConstraint.add_literal (Neg (Fen (y, Feat.Set.union fs gs)))
           |> DXC.singleton)
          (one_difference_in x y (Feat.Set.diff fs gs) conj))
    ()

let e_nfen =
  make
    ~pat:Pattern.[Pos (Sim (x, fs, y)); Neg (Fen (x, gs))]
    ~pred:(fun a _ -> not (Feat.Set.subset (Assign.feat_set a fs) (Assign.feat_set a gs)))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        let conj = XConstraint.add_literal (Pos (Sim (x, fs, y))) conj in
        DXC.or_
          (conj
           |> XConstraint.add_literal (Neg (Fen (x, Feat.Set.union fs gs)))
           |> DXC.singleton)
          (one_feature_in x (Feat.Set.diff fs gs) conj))
    ()

let e_nsim =
  make
    ~pat:[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, z))]
    ~pred:(fun a _ -> not (Feat.Set.subset (Assign.feat_set a fs) (Assign.feat_set a gs)))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let z = Assign.var a z in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        let conj = XConstraint.add_literal (Pos (Sim (x, fs, y))) conj in
        DXC.or_
          (conj
           |> XConstraint.add_literal (Neg (Sim (x, Feat.Set.union fs gs, z)))
           |> DXC.singleton)
          (one_difference_in x z (Feat.Set.diff fs gs) conj))
    ()

let p_nfen =
  make
    ~pat:[Pos (Sim (x, fs, y)); Neg (Fen (x, gs))]
    ~pred:(fun a _ -> Feat.Set.subset (Assign.feat_set a fs) (Assign.feat_set a gs))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        conj
        |> XConstraint.add_literals_list
          [ Pos (Sim (x, fs, y));
            Neg (Fen (x, gs));
            Neg (Fen (y, gs)) ]
        |> DXC.singleton)
    ()

let p_nsim =
  make
    ~pat:[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, z))]
    ~pred:(fun a _ -> Feat.Set.subset (Assign.feat_set a fs) (Assign.feat_set a gs))
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let z = Assign.var a z in
        let fs = Assign.feat_set a fs in
        let gs = Assign.feat_set a gs in
        conj
        |> XConstraint.add_literals_list
          [ Pos (Sim (x, fs, y));
            Neg (Sim (x, gs, z));
            Neg (Sim (y, gs, z)) ]
        |> DXC.singleton)
    ()

let s_kind =
  make
    ~pat:[Pos (Kind (x, k))]
    ~pred:(fun a _ -> Assign.kind a k <> Dir)
    ~prod:(fun a conj ->
        let x = Assign.var a x in
        let k = Assign.kind a k in
        conj
        |> XConstraint.add_literals_list
          [ Pos (Fen (x, Feat.Set.empty));
            Pos (Kind (x, k))]
        |> DXC.singleton)
    ()

(* ========================================================================== *)
(* ============================= [ All Rules ] ============================== *)
(* ========================================================================== *)

let all = [
    "C-Cycle",      c_cycle;
    "C-Feat-Abs",   c_feat_abs;
    "C-Feat-Fen",   c_feat_fen;
    "C-NEq-Refl",   c_neq_refl;
    "C-NSim-Refl",  c_nsim_refl;
    "C-Kinds",      c_kinds;
    "S-Eq-Glob",    s_eq_glob;
    "S-Eq",         s_eq;
    "S-Eq-Refl",    s_eq_refl;
    "S-Feats",      s_feats;
    "S-Feats-Glob", s_feats_glob;
    "S-Sims",       s_sims;
    "S-Kind",       s_kind;
    "P-Feats",      p_feat;
    "P-Abs",        p_abs;
    "P-Fen",        p_fen;
    "P-Sim",        p_sim;
    "R-Neq",        r_neq;
    "R-NFeat",      r_nfeat;
    "R-NAbs",       r_nabs;
    "R-NFen-Fen",   r_nfen_fen;
    "R-NSim-Sim",   r_nsim_sim;
    "R-NSim-Fen",   r_nsim_fen;
    "R-NKind",      r_nkind;
    "E-NFen",       e_nfen;
    "E-NSim",       e_nsim;
    "P-NFen",       p_nfen;
    "P-NSim",       p_nsim;
  ]
