(** 
   Representation of dynamic game state.

   This module represents the state of a grid as it is being played,
   including the agent's current position and orientation, the current color of
   each square and functions that cause the state to change.
*)


(** The abstract type of values representing the game state. *)
type t 

(** Raised when a move cannot be performed. *)
exception Invalid_move

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
