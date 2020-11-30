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
