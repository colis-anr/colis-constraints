include BatList

let concat_map f l =
  let rec aux f acc = function
    | [] -> rev acc
    | x :: l ->
      let xs = f x in
      aux f (rev_append xs acc) l
  in aux f [] l

let rec insert_uniq_sorted cmp e = function
  | [] -> [e]
  | h :: q ->
    match cmp h e with
    | c when c < 0 -> h :: insert_uniq_sorted cmp e q
    | c when c = 0 -> h :: q
    | _            -> e :: h :: q
