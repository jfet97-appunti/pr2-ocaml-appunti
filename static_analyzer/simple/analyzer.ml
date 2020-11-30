let rec closedin ex l =
  match ex with
    | CstInt(_) -> true
    | Sum(e1, e2) -> closedin e1 l && closedin e2 l
    | Times(e1, e2) -> closedin e1 l && closedin e2 l
    | Den(i) -> List.exists (fun y -> i = y) l
    | Let(i, e, b) -> let l2 = i::l in
      closedin e l && closedin b l2;;

let closed e = closedin e [];;