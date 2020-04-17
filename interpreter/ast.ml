(** The type of commands in language *)


type identifier = string

type command =
  | Move
  | Right
  | Left
  | Color of int
  | App of identifier

type definition = Fun of (identifier * command list)

(** The type of the abstract syntax tree (AST) *)
type program = (definition list * command list)