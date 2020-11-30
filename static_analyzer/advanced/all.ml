type ide = string;;

type exp =
  | CstInt of int
  | Sum of exp * exp
  | Times of exp * exp
  | Den of ide
  | Let of ide * exp * exp;;


type ideB = int;;

type expB =
  | CstIntB of int
  | SumB of expB * expB
  | TimesB of expB * expB
  | DenB of ideB
  | LetB of expB * expB;;

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


let rec eval ex vs =
  match ex with
  | CstIntB(n) -> n
  | SumB(e1, e2) -> (eval e1 vs) + (eval e2 vs)
  | TimesB(e1, e2) -> (eval e1 vs) * (eval e2 vs)
  | DenB(n) -> List.nth vs n
  | LetB(e, b) -> let ev = eval e vs in
      eval b (ev::vs);;

(*
let myy = Let("y", Times(Den("x"), CstInt(10)), Sum(Den("x"), Den("y")));;
let myx = Let("x", CstInt(8), myy);;

let o = compile myy [];; ERRORE 
let o = compile myx [];;
eval o [];;
*)