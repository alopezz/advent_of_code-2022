type monkey_job =
  [ `Value of int
  | `Human
  | `Mul of string * string
  | `Div of string * string
  | `Sum of string * string
  | `Sub of string * string]
