let rec get_index x xs =
  match xs with
  | [] -> failwith("Variable not found (unbound identifier)")
  | y::ys -> if x = y then 0 else 1 + get_index x ys;;

let rec compile ex is =
  match ex with
  | CstInt(n) -> CstIntB(n)
  | Sum(e1, e2) -> SumB(compile e1 is, compile e2 is)
  | Times(e1, e2) -> TimesB(compile e1 is, compile e2 is)
  | Den(i) -> DenB(get_index i is)
  | Let(i, e, b) -> LetB(compile e is, compile b (i::is));;

