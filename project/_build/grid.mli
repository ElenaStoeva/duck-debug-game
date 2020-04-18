(** 
   Representation of static grid data.

   This module represents the data stored in grid files, including the initial 
   and the final state of the grid as well as the initial position of the agent.
     It handles loading of that data from JSON as well
   as querying the data.
*)

(** The abstract type of values representing grids. *)
type t

(** The type of a square*)
type square

(** The type of colors *)
type color = string

(** The type of the agent's orientation *)
type orientation = N|S|E|W

(** The exception that is raised when the orientation string is not valid. *)
exception Invalid_orient

(** [from_json j] is the grid that [j] represents.
    Requires: [j] is a valid JSON grid representation. *)
val from_json : Yojson.Basic.t -> t

(** [get_agent_x] is the x-coordinate of the initial position of the agent *)
val get_agent_x : t -> int

(** [get_agent_y] is the y-coordinate of the initial position of the agent *)
val get_agent_y : t -> int

(** [get_agent_orien] is the initial orientation of the agent *)
val get_agent_orien : t -> orientation

(** [get_start_grid] is the initial grid *)
val get_start_grid : t -> square list

(** [get_size g] is the size of the grid *)
val get_size : t -> int