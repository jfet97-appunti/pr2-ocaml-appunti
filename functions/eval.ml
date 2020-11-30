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


let rec eval (ex, dcl) =
  match ex with
  | CstInt i -> Int(i)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | Iszero e -> is_zero(eval(e, dcl))
  | Eq(e1, e2) -> int_eq(eval(e1, dcl), eval(e2, dcl))
  | Times(e1, e2) -> int_times(eval(e1, dcl), eval(e2, dcl))
  | Sum(e1, e2) -> int_plus(eval(e1, dcl), eval(e2, dcl))
  | Sub(e1, e2) -> int_sub(eval(e1, dcl), eval(e2, dcl))
  | And(e1, e2) -> bool_and(eval(e1, dcl), eval(e2, dcl))
  | Or(e1, e2) -> bool_or(eval(e1, dcl), eval(e2, dcl))
  | Not e -> bool_not(eval(e, dcl))
  | Ifthenelse(cond, ife, elsee) ->
      let c = eval(cond, dcl) in
        (match (typecheck("bool", c), c) with
          | (true, Bool(true)) -> eval(ife, dcl)
          | (true, Bool(false)) -> eval(elsee, dcl)
          | (_, _) -> failwith("nonboolean guard"))
  | Val(_) -> failwith("unbound name")
  | Apply(f, a) -> let d = getFunDef(f, dcl) in
    eval(substitute(a, getFunArg(d), getFunBodyExp(d)), dcl);;


(*

let dbl = Fun(Fid("double"), Ide("x"), Body(Times(Val("x"), CstInt 2)));;
let quad = Fun(Fid("quad"), Ide("x"), Body(Apply(Fid "double", Apply(Fid "double", Val("x")))));;
let dcl = [dbl; quad];;

let exp = Apply(Fid "quad", CstInt 10);;
eval(exp, dcl);;


*)