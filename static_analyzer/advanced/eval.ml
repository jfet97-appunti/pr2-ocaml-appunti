let rec eval ex vs =
  match ex with
  | CstIntB(n) -> n
  | SumB(e1, e2) -> (eval e1 vs) + (eval e2 vs)
  | TimesB(e1, e2) -> (eval e1 vs) * (eval e2 vs)
  | DenB(n) -> List.nth vs n
  | LetB(e, b) -> let ev = eval e vs in
      eval b (ev::vs);;