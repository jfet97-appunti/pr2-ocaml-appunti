type evT =
  | Int of int
  | Bool of bool
  | Closure of ide * exp * env
  | RecClosure of ide * ide * exp * env
  | Unbound;;