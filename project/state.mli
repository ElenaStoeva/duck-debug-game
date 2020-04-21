(** 
   Representation of dynamic game state.

   This module represents the state of a grid as it is being played,
   including the agent's current position and orientation, the current color of
   each square and functions that cause the state to change.
*)


(** The abstract type of values representing the game state. *)
type t 

(** Raised when a move cannot be performed. *)
exception Invalid

type direction = Right | Left

(** [init_state g] is the initial state of the game when playing grid [g]. 
    In that state the agent is currently located in its initial square,
    and they grid has its initial state as well. *)
val init_state : Grid.t -> t

(** [move st] changes the position of the agent and returns a new state.*)
val move : t -> t

(** [turn command st] changes the orientation of the agent and returns a new state.*)
val turn : direction -> t ->  t

(** [color command st] changes the orientation of the agent and returns a new state.*)
val color : Grid.color -> t -> t

