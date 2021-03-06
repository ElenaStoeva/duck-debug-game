(** 
   Representation of dynamic game state.

   This module represents the state of a grid as it is being played,
   including the agent's current position and orientation, the current color of
   each square and functions that cause the state to change.
*)


(** The abstract type of values representing the game state. *)
type t 

(** Raised when the player tries to go off the grid. *)
exception Invalid_move

(** Raised when the player tries to step on a wall. *)
exception Wall_exception

(** The type of a possible turn command. *)
type direction = Right | Left

(** [init_state g] is the initial state of the game when playing grid [g]. 
    In that state the agent is currently located in its initial square,
    and they grid has its initial state as well. *)
val init_state : Grid.t -> t

(** [move st] changes the position of the agent and returns a new state. 
    Raises [Invalid_move] if the move is not within the grid bounds. *)
val move : t -> t

(** [turn command st] is state [st] with the orientation of the agent changed 
    in the [command] direction. *)
val turn : direction -> t ->  t

(** [color_square x y cl st] is the state [st] with the square located in 
    [x],[y] changed to attribute [cl]. *)
val color_square : int -> int -> Grid.attribute -> t -> t

(** [color cl st] is the state [st] with the square located in the agents
    location changed to attribute [cl]. *)
val color : Grid.attribute -> t -> t

(** [to_list st] is the list representation of the grid in [st]. *)
val to_list : t -> (int * int * Grid.attribute) list

(** [check_win st g] is true if all the squares in the winning state of the 
    grid are in the current grid *)
val check_win : t -> Grid.t -> bool

(** [get_agent st] is the pair representing the [x,y] coordinates of the 
    agent in state [st]. *)
val get_agent : t -> int * int * Grid.orientation

(** [get_current_color st] is the color of the square where the agent is 
    currently located. Used for testing.*)
val get_current_color : t -> Grid.attribute

(** [get_steps st] is the number of remaining steps. *)
val get_steps : t -> int