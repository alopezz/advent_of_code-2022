
type tile =
  | Wall
  | Open
  | Empty

type map = tile array array


type instruction =
  | Move of int
  | TurnLeft
  | TurnRight

type path = instruction list
