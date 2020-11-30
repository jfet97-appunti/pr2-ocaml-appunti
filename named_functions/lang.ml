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
