type ideB = int;;

type expB =
  | CstIntB of int
  | SumB of expB * expB
  | TimesB of expB * expB
  | DenB of ideB
  | LetB of expB * expB;;