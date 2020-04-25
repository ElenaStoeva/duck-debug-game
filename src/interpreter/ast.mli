(** The abstract syntax tree description of the fgame language. *)


(** The type of commands in language *)
type command =
  | Move
  | Right
  | Left
  | Color of int
  | App of string (* identifier *)

(** Type of a function definition. *)
type definition = Def of string * (command list)

(** The type of the abstract syntax tree (AST) *)
type program = Prog of (definition list) * (command list)
