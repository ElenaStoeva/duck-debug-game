(** 
   Representation of static grid data.

   This module represents the data stored in grid files, including the initial 
   and the final state of the grid as well as the initial position of the agent.
   It handles loading of that data from JSON as well
   as querying the data. The grid is stored with the origin in the lower left
   corner and a a vertical positive y, horizonatal positive x direction.
*)

(** The abstract type of values representing grids. *)
type t

(** The type of attributes. *)
type attribute = Red | Green | Blue | Wall

(** The type of a square. *)
type square = {
  square_x : int;
  square_y : int;
  attribute : attribute
}

(** The type of the agent's orientation *)
type orientation = N | S | E | W

(** The exception that is raised when the orientation string is not valid. *)
exception Invalid_orient

(** The exception that is raised when the attribute string is not valid. *)
exception Invalid_attribute

(** [from_json j] is the grid that [j] represents.
    Requires: [j] is a valid JSON grid representation. *)
val from_json : Yojson.Basic.t -> t

(** [get_agent_x] is the x-coordinate of the initial position of the agent *)
val get_agent_x : t -> int

(** [get_agent_y] is the y-coordinate of the initial position of the agent *)
val get_agent_y : t -> int

(** [get_agent_orien] is the initial orientation of the agent *)
val get_agent_orien : t -> orientation

(** [get_start_grid g] is the initial grid *)
val get_start_grid : t -> square list

(** [get_winning_grid g]  is a list of squares that form the final shape. *)
val get_winning_grid : t -> square list

(** [get_size g] is the size of the grid *)
val get_size : t -> int

(** [get_square_x s] is the x-coordinate of square [s] *)
val get_square_x : square -> int

(** [get_square_y s] is the y-coordinate of square [s] *)
val get_square_y : square -> int

(**[get_square_att s] is the attribute of square [s] *)
val get_square_att : square -> attribute

(** [update_square_att s a] is the square [s] but with 
    attribute [a]. *)
val update_square_att : square -> attribute -> square

(** [get_instructions g] is a string which explains to the player what they
    have to draw. *)
val get_instructions : t -> string

(** [win_to_list gr] is the list representation of the winning condition in 
    [gr]. The state [st] is winning if every member of this list are in 
    [to_list st]. *)
val win_to_list : t -> (int * int * attribute) list

(** [get_att_from_coord squares x y] is the attribute of square with 
    coordinates x and y*)
val get_att_from_coord : square list -> int -> int -> attribute

(** [get_score g] is the initial number from which we subtract the length of the
    player's command to get their final score.*)
val get_score : t -> int

(** [max_steps g] is the maximum number of steps that the player can make. *)
val get_max_steps : t -> int