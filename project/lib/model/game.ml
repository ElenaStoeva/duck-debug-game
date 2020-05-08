type t = {
  id : int;
  highest_score : int;
  highest_level : int;
  levels : string list
}

let from_json json = {
  id = Yojson.Basic.Util.(json |> member "id" |> to_int);
  highest_score = Yojson.Basic.Util.(json |> member "highest_score" |> to_int);
  highest_level = Yojson.Basic.Util.(json |> member "highest_level" |> to_int);
  levels = Yojson.Basic.Util.(json |> member "levels" |> to_list |> List.map to_string);
}

let get_id game = game.id

let get_highest_score game = game.highest_score

let get_highest_level game = game.highest_score

let get_levels game = game.levels

let create_game id score level levels = {
  id = id;
  highest_score = score;
  highest_level = level;
  levels = levels;
}