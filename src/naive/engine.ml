open Colis_constraints_common

let (x, y, z) = Metavar.fresh3 ()
let (f, g) = Metavar.fresh2 ()
let (fs, gs) = Metavar.fresh2 ()

let apply_rule_on_disj (name, rule) disj =
  let applied = ref false in
  let apply_rule_on_conj rule conj =
    match rule conj with
    | None -> DXC.singleton conj
    | Some disj' ->
      if not !applied then
        (
          Log.debug (fun m -> m "Rule %s applied" name);
          applied := true
        );
      disj'
  in
  let disj' = DXC.concat_map (apply_rule_on_conj rule) disj in
  if !applied then Some disj' else None

let rec apply_rules_on_disj rules disj =
  match rules with
  | [] -> None
  | rule :: rules ->
     match apply_rule_on_disj rule disj with
     | None -> apply_rules_on_disj rules disj
     | Some disj' -> Some disj'

let is_atom_about xs = function
  | Atom.Abs (a, _) | Kind (a, _) | Fen (a, _) ->
     Var.Set.mem a xs
  | Eq (a, b) | Feat (a, _, b) | Maybe (a, _, b) | Sim (a, _, b) ->
     Var.Set.mem a xs || Var.Set.mem b xs

let is_literal_about xs = function
  | Literal.Pos a | Neg a -> is_atom_about xs a

let remove_literals_about_in_literal_set xs =
  Literal.Set.filter (fun l -> not (is_literal_about xs l))

let rec normalize limit d =
  Log.debug (fun m -> m "%a" DXC.pp d);
  assert (limit >= 0);
  match apply_rules_on_disj Rules.all d with
  | None ->
     d
  | Some d ->
     normalize (limit-1) d

let normalize ?(limit=50) (disj : DXC.t) : DXC.t =
  Log.debug (fun m -> m "Normalizing");
  let disj' = normalize limit disj in
  Log.debug (fun m -> m "Normal form reached.");
  disj'

let simplify (disj : DXC.t) : DXC.t =
  Log.debug (fun m -> m "Simplifying");
  Limits.check_cpu_time_limit ();
  Limits.check_memory_limit ();
  let disj' =
    DXC.map
      (fun conj ->
         let es = XConstraint.quantified_variables_set conj in
         let c = XConstraint.literals_set conj in
         let xs =
           Rules.accessibility c
           |> List.filter (fun (x, ys) ->
               Var.Set.mem x es && Var.Set.subset ys es)
           |> List.map (fun (x, _) -> x)
           |> Var.Set.of_list
         in
         XConstraint.from_sets
           (Var.Set.diff es xs)
           (remove_literals_about_in_literal_set xs c))
      disj
  in
  Log.debug (fun m -> m "%a" DXC.pp disj');
  DXC.to_seq disj'
  |> Seq.iter
    (fun conj' ->
       Log.debug (fun m -> m "%a" (XConstraint.pp_as_dot ~name:"sdlkfj") conj'));
  disj'
