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
  size : int;
  agent: agent;
  start_grid : square list;
  final_grid : square list;
}

let attribute_of_string color = match color with
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | "wall" -> Wall
  | _ -> raise Invalid_attribute

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

let orientation_of_string orient = match orient with
  | "N" -> N
  | "S" -> S
  | "E" -> E
  | "W" -> W
  | _ -> raise Invalid_orient

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

let get_size g = g.size

let get_square_x s = s.square_x

let get_square_y s = s.square_y

let get_square_att s = s.attribute

let update_square_att s att = {
  s with
  attribute = att;
  }