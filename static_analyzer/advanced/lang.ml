type ide = string;;

type exp =
  | CstInt of int
  | Sum of exp * exp
  | Times of exp * exp
  | Den of ide
  | Let of ide * exp * exp;;