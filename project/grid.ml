open Yojson

type color = string

type orientation = string

type square = {
  x : int;
  y : int;
  color: string;
  obstacle: bool;
}

type agent = {
  x : int;
  y: int;
  orientation: orientation;
  color: string;
}

type t = {
  size : int;
  obstacle_color : color;
  agent: agent;
  start_grid : square list;
  final_grid : square list;
}

let square_of_json json = {
  x = Yojson.Basic.Util.(json |> member "x" |> to_int);
  y = Yojson.Basic.Util.(json |> member "y" |> to_int);
  color = Yojson.Basic.Util.(json |> member "color" |> to_string);
  obstacle = Yojson.Basic.Util.(json |> member "obstacle" |> to_bool);
}

let agent_of_json json = {
  x = Yojson.Basic.Util.(json |> member "x" |> to_int);
  y = Yojson.Basic.Util.(json |> member "y" |> to_int);
  orientation = Yojson.Basic.Util.(json |> member "orientation" |> to_string);
  color = Yojson.Basic.Util.(json |> member "color" |> to_string);
}

let from_json json = {
  size = Yojson.Basic.Util.(json |> member "size" |> to_int);
  obstacle_color = Yojson.Basic.Util.(json |> member "obstacle_color" |> to_string);
  agent = agent_of_json json;
  start_grid = Yojson.Basic.Util.(json |> member "start_squares" |> to_list |> List.map square_of_json);
  final_grid = Yojson.Basic.Util.(json |> member "winning_squares" |> to_list |> List.map square_of_json);
}

let get_agent_x g = g.agent.x

let get_agent_y g = g.agent.y

let get_agent_orien g = g.agent.orientation

let get_start_grid g = g.start_grid

let get_size g = g.size
