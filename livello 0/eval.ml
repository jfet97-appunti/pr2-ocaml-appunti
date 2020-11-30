let rec eval (e: exp) : int =
  match e with
  | CstInt i -> i
  | Sum(e1, e2) -> let v1 = eval e1 in
                    let v2 = eval e2 in
                      v1 + v2
  | Times(e1, e2) -> let v1 = eval e1 in
                      let v2 = eval e2 in
                        v1 * v2;;

let eval_ext (e: exp_ext) : int = eval (translate e);;