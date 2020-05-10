(** View interface for CLI for displaying game. Called by executable cli_app.*)

(** [print_grid st gr] displays game with grid [gr] in the current game state 
    [st], along with the winning state. *)
val print_grid : Model.State.t -> Model.Grid.t -> unit
