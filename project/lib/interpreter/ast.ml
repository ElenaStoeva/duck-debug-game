(** The abstract syntax tree description of the game language. *)


(** The type of commands in language *)
type command =
  | Move
  | Right
  | Left
  | Color of int
  | ConstApp of string
  | FunApp of string * (command list list)
  | VarApp of string (* only in function defs *)

(** Type of a variable. *)
type variable = Var of string

(** Type of a function definition. *)
type definition = 
  | Const of string * command list
  | Def of string * (variable list) * (command list)

(** The type of the abstract syntax tree (AST) *)
type program = Prog of (definition list) * (command list)
