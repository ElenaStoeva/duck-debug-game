let () = 
  if Sys.argv.(1) = "cli" then Cli_app.main () else Gui_app.main ()