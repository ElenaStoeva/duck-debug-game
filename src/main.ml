(** [read_user_code st gr] is a move stream resulting from parsing user's 
    solution code for the level.
    Requires: [st] is a valid game state and [gr] is a valid grid. *)
let rec read_user_code st gr =
  Print.print_grid st gr;
  print_string "\nInput code:\n";
  match read_line () with
  | str -> begin try str |> Eval.parse |> Eval.init_stream 
      with e -> 
        print_endline "\nInterpreting error. Please try again.\n"; 
        read_user_code st gr end

(** [match_move st] is the new game state after agent has moved, and [st] if 
    moving agent raises exception. *)
let match_move st = match State.move st with
  | exception e -> st
  | new_st -> new_st

(** [color_of_int i] is the grid attribute corresponding to [i]. 
    Raises: [Failure] if [i] does not match valid color. *)
let color_of_int i = match i with
  | 1 -> Grid.Red
  | 2 -> Grid.Green
  | 3 -> Grid.Blue
  | _ -> failwith "Non-defined color"

(** [run_simulation st gr ms] prompts user and steps simulation of move stream 
    [ms] in game with valid grid [gr] and valid state [st], until player wins, 
    quits, or player code terminates.
    Raises: [Malformed_stream] if head of move stream created from user code 
    does not match valid command.  *)
let rec run_simulation st gr ms =
  Print.print_grid st gr;
  let hd_opt = try Some (Eval.hd ms) 
    with e -> None in
  if State.check_win st gr then print_endline "\nCongratulations. You won!."
  else if hd_opt =  None 
  then print_endline "Your code terminated but you did not win :(\n"
  else 
    let _ = print_string "\nPress n to step the simulation and q to quit.\n" in
    let tl = Eval.tl ms in
    match read_line () with
    | "q" -> ()
    | "s" -> () (* TODO: stop simulation *)
    | "n" -> begin match hd_opt with
        | Some Eval.M -> run_simulation (match_move st) gr tl
        | Some Eval.R -> run_simulation (State.turn State.Right st) gr tl
        | Some Eval.L -> run_simulation (State.turn State.Left st) gr tl
        | Some Eval.C (c) -> begin 
            run_simulation (State.color (color_of_int c) st) gr tl
          end
        | None -> failwith "Move stream malformed."
      end
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
  | "1" -> st_gr "json_files/level1.json"
  | "2" -> st_gr "json_files/level2.json"
  | "3" -> st_gr "json_files/example.json"
  | "q" -> ()
  | _-> begin 
      print_string "\nUnrecognized level. Please enter valid command: \n"; 
      init_game () end

(** [main ()] prompts for the game to play, displays welcome message, and
    starts the game. *)
let main () =
  print_string "Welcome. Please enter a level number (1,2,3) to play \
                or (q) to quit: \n";
  init_game ()

(* Execute the game engine. *)
let () = main ()