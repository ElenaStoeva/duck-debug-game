open Yojson.Basic.Util

type t = Game.t list


let from_json json = 
  json |> member "games" |> to_list |> List.map Game.from_json

let rec helper_find_game list id = 
  match list with
  | [] -> None
  | h::t -> if (Game.get_id h) = id then Some h else helper_find_game t id

let find_game t id = helper_find_game t id

let rec helper_update list id score level acc =
  match list with 
  | [] -> acc
  | h::t -> if Game.get_id h = id then helper_update t id score level ((Game.create_game id score level (Game.get_levels h))::acc)
    else helper_update t id score level (h::acc)

let update list id score level =
  helper_update list id score level []

let game_to_string game = 
  "{ \"id\" : " ^ (Game.get_id game |> Int.to_string )^ ",\n
   \"heighest_score\" : " ^ (Game.get_highest_level game |> Int.to_string )^ ",\n
   \"heighest_level\" : " ^ (Game.get_highest_level game |> Int.to_string )^ ",\n
   \"levels\" : [" ^ (String.concat ",\n" (Game.get_levels game))^ "]\n}"

let to_string t =  List.map game_to_string t |> String.concat ",\n" 

let new_json id score level = 
  let games = from_json (Yojson.Basic.from_file "progress.json") in
  let new_pack =
    match find_game games id with
    | Some game -> let game_score = Game.get_highest_score game in
      let game_level = Game.get_highest_level game in
      if game_score < score && game_level < level then update games id score level
      else if Game.get_highest_score game < score then update games id score game_level
      else if Game.get_highest_level game < level then update games id game_score level
      else games
    | None -> games
  in to_string new_pack