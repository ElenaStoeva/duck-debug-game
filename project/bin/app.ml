let () = 
  let instr = 
    "Welcome.\n\
     Game objective:\nWrite code to move agent and achieve winning state.\n\
     Game language syntax:\n\
     'M' - Move Forward\n'R' - Turn Right\n'L' - Turn Left\n\
     '1' - Color Square Red\n'2' - \
     Color Square Green\n'3' - Color Square Blue\n\
     'f=' - define function f\n'[f]' - call function f\n\
     ';' - seperator between function definitions.\n\
     Please enter a level number (1,2,3) to play, or (q) to quit: \n" in
  if Sys.argv.(1) = "cli" then Cli_app.main instr else Gui_app.main instr