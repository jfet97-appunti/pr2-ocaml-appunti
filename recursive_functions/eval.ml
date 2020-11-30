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

let rec eval ex ev =
  match ex with
  | CstInt i -> Int(i)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | Eq(e1, e2) -> int_eq((eval e1 ev), (eval e2 ev))
  | Times(e1, e2) -> int_times((eval e1 ev), (eval e2 ev))
  | Sum(e1, e2) -> int_plus((eval e1 ev), (eval e2 ev))
  | Sub(e1, e2) -> int_sub((eval e1 ev), (eval e2 ev))
  | Ifthenelse(cond, ife, elsee) ->
      let c = eval cond ev in
        (match (typecheck("bool", c), c) with
          | (true, Bool(true)) -> eval ife ev
          | (true, Bool(false)) -> eval elsee ev
          | (_, _) -> failwith("nonboolean guard"))
  | Den(i) -> ev i
  | Let (i, e, b) -> eval b (bind ev i (eval e ev))
  (* valuto l'espressione e con l'ev U al binding i -> closure ricorsiva *)
  | LetRec (i, fp, b, e) -> eval e (bind ev i (RecClosure(i, fp, b, ev)))
  (* quando creo una funzione non faccio altro che salvare l'ambiente creato fino a li = cClosure *)
  | Fun(fp, b) -> Closure(fp, b, ev)
  (*
  ev: environment dove viene valutata la Apply
  f: identificatore della funzione
  ape: l'espressione che, se valutata (in ev), darà il valore del parametro attuale da passare ad f
  c: closure derivante dall'aver cercato f nell'environment ev

  fp: il parametro formale di c
  b: il corpo di c
  cev: l'environment di c (quello dove la closure era stata creata)
  ap: valore del parametro attuale da passare ad f
  aev: environment da usare quando si valuta il body di f, costituito da cev U al binding fp -> ap

  rn: nome della closure ricorsiva
  rfp: il parametro formale di c
  rb: il corpo di c
  rcev: l'environment di c (quello dove la closure era stata creata)
  raev: environment da usare quando si valuta il body di f aka rn, costituito da cev U al binding fp -> ap
  *)
  | Apply(Den(f), ape) ->
      let c = ev f in
        (match c with
          | Closure(fp, b, cev) ->
            let ap = eval ape ev in
            let aev = bind cev fp ap in
            eval b aev
          | RecClosure(rn, rfp, rb, rcev) ->
            let ap = eval ape ev in
            let raev = bind (bind rcev rfp ap) rn c in
            eval rb raev
          | _ -> failwith("Application: not a functional value")
        )
  | Apply(_, _) -> failwith("Application: the expression is not an identifier");;