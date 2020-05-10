open GMain

(** [handle_next gui ()] is the handler function for the button [next].
    It has the side effect of advancing the simulation by 1 step and
    updating the display of the game in [gui]. *)
let handle_next (gui : View.Gui.t) () =
  if gui.program_loaded then 
    let _ = match Controller.next gui.ct with
      | Gameover m -> View.Gui.message gui ("try again "^m)
      | Winning i -> View.Gui.message gui 
                       (" You won! Score: "^(string_of_int i)^"/100")
      | Next (ct',m) -> View.Gui.message gui m; gui.ct <- ct' in
    let st = Controller.get_state gui.ct in
    View.Gui.update gui.grid_matrix 
      (st |> Model.State.to_list) 
      (st |> Model.State.get_agent)
  else View.Gui.message gui "you need to compile"

(** [handle_retry gui ()] is the handler function for the button [retry].
    It has the side effect of resetting the simulation in [gui]. *)
let handle_retry (gui : View.Gui.t) () =
  if gui.program_loaded || gui.run then begin
    (* match !th with None -> () | Some x -> Thread.kill x; *)
    gui.run <- false;
    gui.program_loaded <- false;
    gui.textbox#set_editable true;
    gui.ct <- 
      Controller.initialize gui.filename (Controller.get_program gui.ct); 
    let st = Controller.get_state gui.ct in
    View.Gui.update gui.grid_matrix 
      (st |> Model.State.to_list) (st |> Model.State.get_agent)
  end
  else View.Gui.message gui "game already running"

(** [handle_compile gui ()] is the handler function for the button [compile].
    It has the side effect of updating the controller with the program
    written inside the textbox in [gui]. *)
let handle_compile (gui : View.Gui.t) () =
  if gui.program_loaded = false then begin
    gui.program_loaded <- true;
    try 
      gui.ct <- Controller.reset_prog gui.ct (gui.textbox#text);
      gui.textbox#set_editable false;
      let st = Controller.get_state gui.ct in
      View.Gui.update gui.grid_matrix 
        (st |> Model.State.to_list) (st |> Model.State.get_agent);
      View.Gui.message gui "code successfully compiled"
    with Controller.InterpreterError msg -> View.Gui.message gui (msg)
  end
  else View.Gui.message gui "game already running"

(** [thread_aux gui] is the helper function run by the simulation thread
    to periodically advance the simulation. *)
let rec thread_aux (gui : View.Gui.t) = 
  if gui.run = false 
  then View.Gui.message gui "simulation terminated"
  else begin
    View.Gui.message gui "simulation running";
    let b = match Controller.next gui.ct with
      | Gameover m -> View.Gui.message gui ("try again "^m); false
      | Winning i -> 
        View.Gui.message gui 
          (" You won! Score: "^(string_of_int i)^"/100"); false
      | Next (ct',m) -> View.Gui.message gui m; gui.ct <- ct'; true in
    let st = Controller.get_state gui.ct in
    View.Gui.update gui.grid_matrix (st |> Model.State.to_list) 
      (st |> Model.State.get_agent);
    Thread.delay 0.05; 
    if b then thread_aux gui 
    else gui.run <- false
  end

(** [handle_play gui ()] is the handler function for the button [play].
    It has the side effect of starting a new thread executing [thread_aux]. *)
let handle_play (gui : View.Gui.t) () =
  if gui.run then View.Gui.message gui "simulation already running"
  else if gui.program_loaded = false then View.Gui.message gui "compile first"
  else begin gui.run <- true;
    let thr = Thread.create thread_aux gui in
    gui.th <- Some (thr)
  end

(** [main ()] initializes and starts the graphical user interface. *)
let main str =
  print_string str;
  print_endline "\nEnter level number (1,2,3,4,5,6) or (q) to quit:";
  let fl = match read_line () with
    | exception End_of_file -> Stdlib.exit 0
    | "1" -> "resources/json_files/level5.json"
    | "2" -> "resources/json_files/level6.json"
    | "3" -> "resources/json_files/level7.json"
    | "4" -> "resources/json_files/level9.json"
    | "5" -> "resources/json_files/level8.json"
    | "6" -> "resources/json_files/level4.json"
    | "q" -> Stdlib.exit 0 
    | _ -> print_endline "Unrecognized level."; Stdlib.exit 0 in


  let gui = View.Gui.initialize fl (Controller.initialize fl "") in

  gui.window#connect#destroy ~callback:Main.quit |> ignore;
  gui.next#connect#clicked ~callback:(handle_next gui) |> ignore; 
  gui.retry#connect#clicked ~callback:(handle_retry gui) |> ignore;
  gui.compile#connect#clicked ~callback:(handle_compile gui) |> ignore;
  gui.quit#connect#clicked ~callback:Main.quit |> ignore;
  gui.play#connect#clicked ~callback:(handle_play gui) |> ignore;

  View.Gui.update gui.grid_matrix 
    (Controller.get_state gui.ct |> Model.State.to_list) 
    (Controller.get_state gui.ct |> Model.State.get_agent);

  View.Gui.draw_win gui.win_matrix 
    (Controller.get_grid gui.ct) 
    (Controller.get_state gui.ct);

  View.Gui.show gui.window