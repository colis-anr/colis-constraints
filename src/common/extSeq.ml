include BatSeq

let to_list s =
  let rec to_list acc s =
    match s() with
    | Nil -> List.rev acc
    | Cons (x, s) -> to_list (x :: acc) s
  in
  to_list [] s
