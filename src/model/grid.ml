type attribute = Red | Green | Blue | Wall

type orientation = N | S | E | W

exception Invalid_orient

exception Invalid_attribute

type square = {
  square_x : int;
  square_y : int;
  attribute : attribute;
}

type agent = {
  agent_x : int;
  agent_y: int;
  orientation: orientation;
}

type t = {
  instructions: string;
  size : int;
  agent: agent;
  start_grid : square list;
  final_grid : square list;
}

(**[attribute_of_string color] is the type of the attribute represented by
   the string [attribute] *)
let attribute_of_string attribute = match attribute with
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | "wall" -> Wall
  | _ -> raise Invalid_attribute

(**[square_of_json json] is a record of a square that is represented in the 
   json [json] *)
let square_of_json json = {
  square_x = Yojson.Basic.Util.(json |> member "x" |> to_int);
  square_y = Yojson.Basic.Util.(json |> member "y" |> to_int);
  attribute = Yojson.Basic.Util.(
      json 
      |> member "attribute" 
      |> to_string 
      |> attribute_of_string
    );
}

(**[orientation_of_string orient] is the type of the orientation represented
   by the string [orient] *)
let orientation_of_string orient = match orient with
  | "N" -> N
  | "S" -> S
  | "E" -> E
  | "W" -> W
  | _ -> raise Invalid_orient

(**[agent_of_json json] is a record of an agent that is represented in the 
   json [json] *)
let agent_of_json json = {
  agent_x = Yojson.Basic.Util.(json |> member "agent_x" |> to_int);
  agent_y = Yojson.Basic.Util.(json |> member "agent_y" |> to_int);
  orientation = Yojson.Basic.Util.(
      json 
      |> member "orientation" 
      |> to_string 
      |> orientation_of_string
    );
}

let from_json json = {
  instructions = Yojson.Basic.Util.(json |> member "instructions" |> to_string);
  size = Yojson.Basic.Util.(json |> member "size" |> to_int);
  agent = Yojson.Basic.Util.(json |> member "agent" |> agent_of_json);
  start_grid = Yojson.Basic.Util.(
      json 
      |> member "start_squares" 
      |> to_list
      |> List.map square_of_json
    );
  final_grid = Yojson.Basic.Util.(
      json 
      |> member "winning_squares"
      |> to_list 
      |> List.map square_of_json
    );
}

let get_agent_x g = g.agent.agent_x

let get_agent_y g = g.agent.agent_y

let get_agent_orien g = g.agent.orientation

let get_start_grid g = g.start_grid

let get_winning_grid g = g.final_grid

let get_size g = g.size

let get_square_x s = s.square_x

let get_square_y s = s.square_y

let get_square_att s = s.attribute

let update_square_att s att = {
  s with
  attribute = att;
}

let get_instructions g = g.instructions

(** [helper_list gl ac] is the list representation of [gl] added to list [ac].*)
let rec helper_list (glst : square list) acc = match glst with
  | [] -> acc
  | {square_x=x; square_y=y; attribute=a}::t -> helper_list t ((x,y,a)::acc)

let win_to_list gr = helper_list (get_winning_grid gr) []