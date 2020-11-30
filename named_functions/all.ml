type ide = string;;

type exp =
  | CstInt of int
  | CstTrue
  | CstFalse
  | Eq of exp * exp
  | Sum of exp * exp
  | Sub of exp * exp
  | Times of exp * exp
  | Ifthenelse of exp * exp * exp
  | Den of ide
  | Let of ide * exp * exp
  | Fun of ide * exp
  | Apply of exp * exp;;


type env = ide -> evT
    and evT =
    | Int of int
    | Bool of bool
    | Closure of ide * exp * env
    | Unbound;;;;

let emptyEnv = fun _ -> Unbound;;

(*
  bind richiede un ambiente in ingresso ritorna una funzione di lookup
  l'ambiente e' implicito nella funzione di lookup
*)
let bind (a: env) (li:ide) (lv:evT) = (fun i ->
  if li = i then lv
  else a i
);;


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

let int_sub (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Int(i - j)
      | (_, _, _, _) -> failwith("run-time error");;

let rec eval ex ev =
  match ex with
  | CstInt i -> Int(i)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | Eq(e1, e2) -> int_eq((eval e1 ev), (eval e2 ev))
  | Times(e1, e2) -> int_times((eval e1 ev), (eval e2 ev))
  | Sum(e1, e2) -> int_plus((eval e1 ev), (eval e2 ev))
  | Sub(e1, e2) -> int_sub((eval e1 ev), (eval e2 ev))
  | Ifthenelse(cond, ife, elsee) ->
      let c = eval cond ev in
        (match (typecheck("bool", c), c) with
          | (true, Bool(true)) -> eval ife ev
          | (true, Bool(false)) -> eval elsee ev
          | (_, _) -> failwith("nonboolean guard"))
  | Den(i) -> ev i
  | Let (i, e, b) -> eval b (bind ev i (eval e ev))
  | Fun(fp, b) -> Closure(fp, b, ev)
  | Apply(Den(f), ape) ->
      let c = ev f in
        (match c with
          | Closure(fp, b, cev) ->
              let ap = eval ape ev in
                let aev = bind cev fp ap in
                  eval b aev
          | _ -> failwith("Application: not a functional value")
        )
  | Apply(_, _) -> failwith("Application: the expression is not an identifier");;


(*
;;

let e = Let("x", CstInt 5, Let ("f", Fun("z", Sum (Den "z", Den "x")), Apply (Den "f", CstInt 1)));;
eval e emptyEnv;;

eval (Fun("z", Sum (Den "z", Den "x"))) emptyEnv;;
let xc = eval (Let("x", CstInt 5, Fun("z", Sum (Den "z", Den "x")))) emptyEnv;;

let evt = (match xc with
  | Closure(_,_, t) -> t
  | _ -> emptyEnv
);;

evt "x";;
*)