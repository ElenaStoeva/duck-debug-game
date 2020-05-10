(** [InterpreterError] is raised when parsing or evaluation of player code 
    raises an exception.*)
exception InterpreterError of string

(** [t] is the type of controller. *)
type t

(** [result] is the type of game result after each step of simulation.*)
type result = Winning of int | Gameover of string | Next of t * string

(** [initialize filename prog_str] is the controller initialized with
    state and grid from provided [filename], and program and move_stream from
    player code [prog_str] *)
val initialize : string -> string -> t

(** [reset_prog t prog_str] is a new controller with state initial state of 
    level grid from current controller [t], and program and move_stream from 
    player code [prog_str].*)
val reset_prog : t -> string -> t

(** [get_state t] is current game state. State field of [t].*)
val get_state : t -> Model.State.t

(** [get_program t] is the player code. Program field of [t].*)
val get_program : t -> string

(** [get_stream t] is the move stream generated upon evaluating player code. 
    Stream field of [t].*)
val get_stream : t -> Interpreter.Eval.move_stream

(** [get_grid t] is the game grid of current level. Grid field of [t].*)
val get_grid : t -> Model.Grid.t

(** [next t] is the result of stepping once the simulation of player code 
    execution.*)
val next : t -> result