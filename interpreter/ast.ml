(** The type of commands in language *)


(* type identifier = Id of string *)

type command =
  | Move
  | Right
  | Left
  | Color of int
  | App of string (* identifier *)


type definition = Def of string * (command list)

(** The type of the abstract syntax tree (AST) *)
type program = Prog of (definition list) * (command list)
