type ide = string;;

type exp =
  | CstInt of int
  | Sum of exp * exp
  | Times of exp * exp
  | Den of ide
  | Let of ide * exp * exp;;

let rec closedin ex l =
  match ex with
    | CstInt(_) -> true
    | Sum(e1, e2) -> closedin e1 l && closedin e2 l
    | Times(e1, e2) -> closedin e1 l && closedin e2 l
    | Den(i) -> List.exists (fun y -> i = y) l
    | Let(i, e, b) -> let l2 = i::l in
      closedin e l && closedin b l2;;

let closed e = closedin e [];;

(*
let myy = Let("y", Times(Den("x"), CstInt(10)), Sum(Den("x"), Den("y")));;
let myx = Let("x", CstInt(8), myy);;

let myweirdbutfalse = Let("x", Times(Den("x"), CstInt(10)), CstInt(8));;
let myweirdbuttrue = Let("x", CstInt(10), Den("x"));;

closed myy;;
closed myx;;
closed myweirdbutfalse;;
closed myweirdbuttrue;;
*)