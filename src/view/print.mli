(** View interface for displaying game. *)

(** [print_grid st gr] displays game with grid [gr] in the current game state 
    [st], along with the winning state. *)
val print_grid : State.t -> Grid.t -> unit