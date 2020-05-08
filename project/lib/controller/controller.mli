exception InterpreterError of string


type t

type result = Winning of int | Gameover of string | Next of t * string


val initialize : string -> string -> t

val reset_prog : t -> string -> t

val get_state : t -> Model.State.t
val get_program : t -> string
val get_stream : t -> Interpreter.Eval.move_stream
val get_grid : t -> Model.Grid.t

val next : t -> result

