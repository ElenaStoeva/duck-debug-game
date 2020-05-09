open View

(** [read_user_code st gr] is a move stream resulting from parsing user's 
    solution code for the level.
    Requires: [st] is a valid game state and [gr] is a valid grid. *)
let rec read_user_code ct =
  Print.print_grid (Controller.get_state ct) (Controller.get_grid ct);
  print_string "\nInput code:\n";
  try Controller.reset_prog ct (read_line ())
  with Controller.InterpreterError msg ->
    print_endline msg; read_user_code ct


(** [run_simulation st gr ms] prompts user and steps simulation of move stream 
    [ms] in game with valid grid [gr] and valid state [st], until player wins, 
    quits, or player code terminates.
    Raises: [Malformed_stream] if head of move stream created from user code 
    does not match valid command.  *)
let rec run_simulation ct =
  Print.print_grid (Controller.get_state ct) (Controller.get_grid ct);
  print_string "\nPress (n) or (Enter) to step the simulation, \
                (r) to stop simulation and retry, and (q) to quit.\n";
  match read_line () with
  | "q" -> ()
  | "r" -> run_simulation (read_user_code ct)
  | ""
  | "n" -> match_result ct
  | _ -> print_string "\nInvalid command.\n"; run_simulation ct
and match_result ct =
  try
    begin
      let retry () = print_endline "Press (r) to retry, or any key to quit.";
        match read_line () with
        | "r" -> run_simulation (read_user_code ct)
        | _ -> () in 
      match Controller.next ct with
      | Winning i -> 
        print_endline
          ("\nCongratulations. You won! Your score is: "^
           (Int.to_string i)^"/100"); 
        retry ()
      | Gameover s -> print_endline s; retry ()
      | Next (ct',s) -> print_endline s; run_simulation ct'
    end
  with Sys_error msg -> print_endline msg


(** [init_game ()] prompts user to enter level and initializes game with 
    appropriate the appropriate state and grid. *)
let rec init_game () =
  let run fl =
    let ct = Controller.initialize fl "" in
    run_simulation (read_user_code ct) in
  match read_line () with
  | exception End_of_file -> ()
  | "0" -> run "resources/json_files/example.json"
  | "1" -> run "resources/json_files/level1.json"
  | "2" -> run "resources/json_files/level2.json"
  | "3" -> run "resources/json_files/level3.json"
  | "4" -> run "resources/json_files/level4.json"
  | "99" -> run "resources/json_files/level99.json"
  | "q" -> ()
  | _-> begin 
      print_string "\nUnrecognized level. Please enter valid command: \n"; 
      init_game () end


(** [main ()] prompts for the game to play, displays welcome message, and
    starts the game. *)
let main str =
  print_string str;
  init_game ()