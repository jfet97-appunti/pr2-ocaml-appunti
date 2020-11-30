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
  | Ifthenelse of exp * exp * exp;;



(* let e = Sum (CstInt 23, Times (CstInt 5, CstInt 6 ));; *)
