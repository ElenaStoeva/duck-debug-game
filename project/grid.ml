open Yojson

type color = string

type square = {
  x : int;
  y : int;
  color: string;
  rock: bool;
}

type agent = {
  x : int;
  y: int;
  orientation: string;
  color: string
}

type t = {
  size : int;
  start_grid : square list;
  final_grid : 
  agent_pos : (int*int);
  obstacle : bool;
}

let square_of_json json = {
  x = Yojson.Basic.Util.(json |> member "x" |> to_int);
  y = Yojson.Basic.Util.(json |> member "y" |> to_int);
  color = Yojson.Basic.Util.(json |> member "color" |> to_string);
  rock = Yojson.Basic.Util.(json |> member "rock" |> to_bool);
}