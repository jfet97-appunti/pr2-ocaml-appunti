type env = ide -> evT;;

let emptyEnv = fun _ -> Unbound;;

(*
  bind richiede un ambiente in ingresso ritorna una funzione di lookup
  l'ambiente e' implicito nella funzione di lookup
*)
let bind (a: env) (li:ide) (lv:evT) = (fun i ->
  if li = i then lv
  else a i
);;