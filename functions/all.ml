type evT = Int of int | Bool of bool;;

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

type funName = Fid of string;;
type funArg = Ide of string;;

type exp =
  | CstInt of int
  | CstTrue
  | CstFalse
  | Iszero of exp
  | Eq of exp * exp
  | Times of exp * exp
  | Sum of exp * exp
  | Sub of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Ifthenelse of exp * exp * exp
  | Val of string
  | Apply of funName * exp;;


type funBody = Body of exp;;


type funDef = Fun of funName * funArg * funBody;;

type funDecl = funDef list;;

let rec getFunDef (n, l) = match(n, l) with
  | (Fid(i), Fun(Fid(i2), Ide(a), Body(b))::ls) ->
      if i = i2 then Fun(Fid(i2), Ide(a), Body(b))
      else getFunDef(n, ls)
  | (Fid(_), []) -> failwith("unknown function");;

let getFunArg f = match f with
  | Fun(Fid(i), Ide(a), Body(b)) -> Ide(a);;

let getFunBody f = match f with
  | Fun(Fid(i), Ide(a), Body(b)) -> Body(b);;

let getFunBodyExp f = match f with
  | Fun(Fid(i), Ide(a), Body(b)) -> b;;

let rec substitute (s, i, d) =
  match d with
  | CstInt i -> CstInt i
  | CstTrue -> CstTrue
  | CstFalse -> CstFalse
  | Iszero e -> Iszero(substitute(s, i, e))
  | Eq(e1, e2) -> Eq(substitute(s, i, e1), substitute(s, i, e2))
  | Times(e1, e2) -> Times(substitute(s, i, e1), substitute(s, i, e2))
  | Sum(e1, e2) -> Sum(substitute(s, i, e1), substitute(s, i, e2))
  | Sub(e1, e2) -> Sub(substitute(s, i, e1), substitute(s, i, e2))
  | And(e1, e2) -> And(substitute(s, i, e1), substitute(s, i, e2))
  | Or(e1, e2) -> Or(substitute(s, i, e1), substitute(s, i, e2))
  | Not e -> Not(substitute(s, i, e))
  | Ifthenelse(cond, ife, elsee) -> Ifthenelse(substitute(s, i, cond), substitute(s, i, ife), substitute(s, i, elsee))
  | Val(j) ->
      (match i with
       | Ide(ii) ->
           if j = ii then s
           else Val(j))
  | Apply(f, e) -> Apply(f, substitute(s, i, e));;


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

let dbl = Fun(Fid("double"), Ide("x"), Body(Times(Val("x"), CstInt 2)));;
let quad = Fun(Fid("quad"), Ide("x"), Body(Apply(Fid "double", Apply(Fid "double", Val("x")))));;
let dcl = [dbl; quad];;

let exp = Apply(Fid "quad", CstInt 10);;
eval(exp, dcl);;