open Cli
open Gui

let () = 
  let instr = 
    "Welcome to DuckDebug!\n\
     Game objective:\nWrite code to move agent and achieve winning state.\n\
     Game language syntax:\n\
     'M' - Move Forward\n'R' - Turn Right\n'L' - Turn Left\n\
     '1' - Color Square Red\n'2' - \
     Color Square Green\n'3' - Color Square Blue\n\
     'f=' - define function f\n'[f]' - call function f\n\
     'f -x -y = xy' - Define f with two arguments x and y concat \n\
     '[f]' - Call Function f\n\
     '[f -MRMRL -RRR]' - apply f with two arguments\n\
     ';' - Separator between definitions\n\
     Please enter a level number (1,2,3,4,5,6) to play, or (q) to quit: \n" in
  if Sys.argv.(1) = "cli" then Cli_app.main instr else Gui_app.main instr