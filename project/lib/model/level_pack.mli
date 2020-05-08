(** 
   Representation of game data.

   This module represents a game that consists of levels and stores the highest
   score achieved. It also stores the current level of the player.
*)

(** The type of a level *)
type level

(**The type of a game *)
type t

(** [from_json j] is the game that [j] represents.
    Requires: [j] is a valid JSON grid representation. *)
val from_json : Yojson.Basic.t -> t

(** [get_levels game] is the list of levels in this game. *)
val get_levels : t -> level list

(** [get_highest_score game] is the sum of the scores from all completed levels.*)
val get_highest_score : t -> int