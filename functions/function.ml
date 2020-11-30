type funName = Fid of string;;
type funArg = Ide of string;;
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