let rec translate(e: exp_ext) : exp =
  match e with
  | CstIntExt i -> (CstInt i)
  | SumExt(e1, e2) -> Sum(translate (e1), translate (e2))
  | TimesExt(e1, e2) -> Times(translate (e1), translate (e2))
  | Minus(e1, e2) -> Sum(translate (e1), Times(CstInt (-1), translate (e2)));;
  | MinusUnary e1 -> Minus(CstInt 0, translate (e1));;