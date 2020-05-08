type level = string

type t = {
  highest_score : int;
  highest_level : int;
  levels : level list
}

let from_json json = {
  highest_score = Yojson.Basic.Util.(json |> member "highest_score" |> to_int);
  highest_level = Yojson.Basic.Util.(json |> member "highest_level" |> to_int);
  levels = Yojson.Basic.Util.(json |> member "levels" |> to_list |> List.map to_string);
}

let get_levels game = game.levels

let get_highest_score game = game.highest_score