type evT = Int of int | Bool of bool | Unbound;;

let typecheck (x, y) = match x with
  | "int" ->
        (match y with
          | Int (i) -> true
          | _ -> false)
  | "bool" ->
        (match y with
          | Bool (b) -> true
          | _ -> false)
  | _ -> failwith ("not a valid type");;



type ide = string;;

type exp =
  | CstInt of int
  | CstTrue
  | CstFalse
  | Sum of exp * exp
  | Times of exp * exp
  | Ifthenelse of exp * exp * exp
  | Eq of exp * exp
  | Den of ide
  | Let of ide * exp * exp;;



type env = ide -> evT;;

let emptyEnv = fun _ -> Unbound;;

let bind (a: env) (li:ide) (lv:evT) = (fun i ->
  if li = i then lv
  else a i
);;

let is_zero x =
  match (typecheck("int", x), x) with
      | (true, Int(i)) -> Bool(i = 0)
      | (_, _) -> failwith("run-time error");;

let int_eq (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Bool(i = j)
      | (_, _, _, _) -> failwith("run-time error");;

let int_plus (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Int(i + j)
      | (_, _, _, _) -> failwith("run-time error");;

let int_times (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Int(i * j)
      | (_, _, _, _) -> failwith("run-time error");;


let rec eval ex ev =
  match ex with
    | CstInt i -> Int(i)
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Eq(e1, e2) -> int_eq(eval e1 ev, eval e2 ev)
    | Times(e1, e2) -> int_times(eval e1 ev, eval e2 ev)
    | Sum(e1, e2) -> int_plus(eval e1 ev, eval e2 ev)
    | Ifthenelse(cond, ife, elsee) ->
        let c = eval cond ev in
          (match (typecheck("bool", c), c) with
            | (true, Bool(true)) -> eval ife ev
            | (true, Bool(false)) -> eval elsee ev
            | (_, _) -> failwith("nonboolean guard"))
    | Den(i) -> ev i
    | Let(i, e, b) -> eval b (bind ev i (eval e ev));;



(*
let myp = Let("x", CstInt(8), Let("y", Times(Den("x"), CstInt(10)), Sum(Den("x"), Den("y"))));;
eval myp emptyEnv;;
*)

