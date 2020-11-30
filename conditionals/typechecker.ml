let typecheck (x, y) = match x with
  | "int" ->
        (match y with
          | Int (i) -> true
          | _ -> false)
  | "bool" ->
        (match y with
          | Bool (b) -> true
          | _ -> false)
  | _ -> failwith ("not a valid type");;