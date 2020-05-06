open Model
open View
open Interpreter

(** [read_user_code st gr] is a move stream resulting from parsing user's 
    solution code for the level.
    Requires: [st] is a valid game state and [gr] is a valid grid. *)
let rec read_user_code st gr =
  Print.print_grid st gr;
  print_string "\nInput code:\n";
  match read_line () with
  | str -> begin try str |> Eval.parse |> Check.check_ast |> Eval.init_stream
      with _ -> 
        print_endline "\nInterpreting error. Please fix your code.\n"; 
        read_user_code st gr end

(** [match_move st] is the new game state after agent has moved, and [st] if 
    moving agent raises exception. *)
let match_move st = match State.move st with
  | exception _ -> print_string 
                     "\nYou cannot move off the grid. Keep stepping.\n"; st
  | new_st -> new_st

(** [color_of_int i] is the grid attribute corresponding to [i]. 
    Raises: [Failure] if [i] does not match valid color. *)
let color_of_int i = match i with
  | 1 -> Grid.Red
  | 2 -> Grid.Green
  | 3 -> Grid.Blue
  | _ -> failwith "\nNon-defined color. Color command ignored.\n"

(** [match_prim st m] is the state after primitive move [m] is applied.
    Requires that [m] is not [None]. *)
let match_prim st m = match m with
  | Some Eval.M -> match_move st
  | Some Eval.R -> State.turn State.Right st
  | Some Eval.L -> State.turn State.Left st
  | Some Eval.C (c) -> State.color (color_of_int c) st
  | None -> failwith "Move stream malformed."


(** [run_simulation st gr ms] prompts user and steps simulation of move stream 
    [ms] in game with valid grid [gr] and valid state [st], until player wins, 
    quits, or player code terminates.
    Raises: [Malformed_stream] if head of move stream created from user code 
    does not match valid command.  *)
let rec run_simulation st gr ms =
  Print.print_grid st gr;
  let hd_opt = try Some (Eval.hd ms) 
    with _ -> None in
  if State.check_win st gr then print_endline "\nCongratulations. You won!."
  else if hd_opt =  None 
  then print_endline "Your code terminated but you did not win :(\n"
  else 
    let _ = print_string "\nPress (n) or (Enter) to step the simulation \
                          and (q) to quit.\n" in
    let tl = Eval.tl ms in
    match read_line () with
    | "q" -> ()
    (* | "s" -> () TODO: stop simulation *)
    | ""
    | "n" -> begin try run_simulation (match_prim st hd_opt) gr tl
        with Failure msg -> print_string msg; run_simulation st gr tl end
    | _ -> print_string "\nInvalid command.\n"; run_simulation st gr ms

(** [init_game ()] prompts user to enter level and initializes game with 
    appropriate the appropriate state and grid. *)
let rec init_game () =
  let st_gr fl =
    let gr = fl |> Yojson.Basic.from_file |> Grid.from_json in
    let st = State.init_state gr in 
    run_simulation st gr (read_user_code st gr) in
  match read_line () with
  | exception End_of_file -> ()
  | "0" -> st_gr "json_files/example.json"
  | "1" -> st_gr "json_files/level1.json"
  | "2" -> st_gr "json_files/level2.json"
  | "3" -> st_gr "json_files/level3.json"
  | "99" -> st_gr "json_files/level99.json"
  | "q" -> ()
  | _-> begin 
      print_string "\nUnrecognized level. Please enter valid command: \n"; 
      init_game () end

(** [main ()] prompts for the game to play, displays welcome message, and
    starts the game. *)
let main () =
  print_string 
    "Welcome.\n\
     Game objective:\nWrite code to move agent and achieve winning state.\n\
     Game language syntax:\n\
     'M' - Move Forward\n'R' - Turn Right\n'L' - Turn Left\n\
     '1' - Color Square Red\n'2' - Color Square Green\n'3' - Color Square Blue\n\
     'f=' - define function f\n'[f]' - call function f\n\
     ';' - seperator between function definitions.\n\
     Please enter a level number (1,2,3) to play, or (q) to quit: \n";
  init_game ()

(* Execute the game engine. *)
let () = main ()