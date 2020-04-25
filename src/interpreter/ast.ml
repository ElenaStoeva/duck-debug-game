type command =
  | Move
  | Right
  | Left
  | Color of int
  | App of string (* identifier *)

type definition = Def of string * (command list)

type program = Prog of (definition list) * (command list)
