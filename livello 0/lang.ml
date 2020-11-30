type exp =
  | CstInt of int
  | Sum of exp * exp
  | Times of exp * exp;;

type exp_ext =
  | CstIntExt of int
  | SumExt of exp_ext * exp_ext
  | TimesExt of exp_ext * exp_ext
  | Minus of exp_ext * exp_ext
  | MinusUnary of exp_ext;;


(* let e = Sum (CstInt 23, Times (CstInt 5, CstInt 6 ));; *)
(* let e_ext = Minus (CstIntExt 5, CstIntExt 4);; *)