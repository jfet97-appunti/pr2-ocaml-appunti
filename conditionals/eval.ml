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

let int_sub (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Int(i - j)
      | (_, _, _, _) -> failwith("run-time error");;

let bool_and (x,y) =
  match (typecheck("bool", x), typecheck("bool", y), x, y) with
      | (true, true, Bool(i), Bool(j)) -> Bool(i && j)
      | (_, _, _, _) -> failwith("run-time error");;

let bool_or (x,y) =
  match (typecheck("bool", x), typecheck("bool", y), x, y) with
      | (true, true, Bool(i), Bool(j)) -> Bool(i || j)
      | (_, _, _, _) -> failwith("run-time error");;

let bool_not x =
  match (typecheck("bool", x), x) with
      | (true, Bool(i)) -> Bool(not(i))
      | (_, _) -> failwith("run-time error");;


let rec eval_eager (e: exp) : evT =
  match e with
  | CstInt i -> Int(i)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | Iszero e -> is_zero(eval_eager(e))
  | Eq(e1, e2) -> int_eq(eval_eager(e1), eval_eager(e2))
  | Times(e1, e2) -> int_times(eval_eager(e1), eval_eager(e2))
  | Sum(e1, e2) -> int_plus(eval_eager(e1), eval_eager(e2))
  | Sub(e1, e2) -> int_sub(eval_eager(e1), eval_eager(e2))
  | And(e1, e2) -> bool_and(eval_eager(e1), eval_eager(e2))
  | Or(e1, e2) -> bool_or(eval_eager(e1), eval_eager(e2))
  | Not e -> bool_not(eval_eager(e))
  | Ifthenelse(cond, ife, elsee) ->
      let c = eval_eager(cond) in
        match (typecheck("bool", c), c) with
          | (true, Bool(true)) -> eval_eager(ife)
          | (true, Bool(false)) -> eval_eager(elsee)
          | (_, _) -> failwith("nonboolean guard");;

let rec eval_lazy (e: exp) : evT =
  match e with
  | CstInt i -> Int(i)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | Iszero e -> is_zero(eval_lazy(e))
  | Eq(e1, e2) -> int_eq(eval_lazy(e1), eval_lazy(e2))
  | Times(e1, e2) -> int_times(eval_lazy(e1), eval_lazy(e2))
  | Sum(e1, e2) -> int_plus(eval_lazy(e1), eval_lazy(e2))
  | Sub(e1, e2) -> int_sub(eval_lazy(e1), eval_lazy(e2))
  | And(e1, e2) ->
      let e1V = eval_lazy(e1) in
        (match (typecheck("bool", e1V), e1V) with
        | (true, Bool(false)) -> Bool(false)
        | (true, Bool(true)) ->
          let e2V = eval_lazy(e2) in
          (match (typecheck("bool", e2V), e2V) with
            | (true, _) -> e2V
            | (_, _) -> failwith("run-time error"))
        | (_,_) -> failwith("run-time error"))
  | Or(e1, e2) ->
      let e1V = eval_lazy(e1) in
        (match (typecheck("bool", e1V), e1V) with
          | (true, Bool(true)) -> Bool(true)
          | (true, Bool(false)) ->
            let e2V = eval_lazy(e2) in
            (match (typecheck("bool", e2V), e2V) with
              | (true, _) -> e2V
              | (_, _) -> failwith("run-time error"))
          | (_, _) -> failwith("run-time error"))
  | Not e -> bool_not(eval_lazy(e))
  | Ifthenelse(cond, ife, elsee) ->
      let c = eval_lazy(cond) in
        match (typecheck("bool", c), c) with
          | (true, Bool(true)) -> eval_lazy(ife)
          | (true, Bool(false)) -> eval_lazy(elsee)
          | (_, _) -> failwith("nonboolean guard");;

(*

let e1 = Eq(CstInt 5, CstInt 5);;
let e2 = Sum(CstInt 5, CstInt 1);;
let e3 = CstInt 2;;
let e = Ifthenelse(e1, e2, e3);;
eval_eager e;;

let e21 = Bool(true);;
let e22 = CstInt 5;;
let e23 = Or(e1, e2);;
eval_lazy e23;;
eval_eager e23;;

*)