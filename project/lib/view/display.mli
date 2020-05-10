(** 
    Graphical User Interface module for game play. Called by executable gui_app.
*)


(** [t] represents the graphical user interface. It includes fields referring
    each componenets outlined below as:
      app - application object
      window - window object 
      grid_matrix - a matrix representing the grid display 
      win_matrix - a matrix representing the winning display 
      filename - the loaded filename
      textbox - the textbox object
      msg - the textdislay on the left side of the screen
      instr - the text display of game instructions
      play, next, retry, compile, quit - the button objects with the same name
      ct - the game controller
      program_loaded - program loaded flag
      th - simulation thread
      run - true if simulation is running *)
type t = {
  app : Gui_class.window;
  window : GWindow.window;
  grid_matrix : GMisc.image array array;
  win_matrix : GMisc.image array array;
  filename : string;
  textbox : GEdit.entry;
  msg : GText.view;
  instr : GText.view;
  play : GButton.button;
  next : GButton.button;
  retry : GButton.button;
  compile : GButton.button;
  quit : GButton.button;
  mutable ct : Controller.t;
  mutable program_loaded : bool;
  mutable th : Thread.t option;
  mutable run : bool;
}

(** [draw_win m gr st] is unit. It has side effects of updating the winning
    grid representation in matrix [m] according to [gr] and [st]. *)
val draw_win :
  GMisc.image array array ->
  Model.Grid.t -> 
  Model.State.t -> 
  unit

(** [update m slst a] is unit. It has side effects of updating the grid 
    representation in matrix [m] according to grid [slst] and agent [a]. *)
val update : 
  GMisc.image array array ->
  (int * int * Model.Grid.attribute) list ->
  int * int * Model.Grid.orientation -> unit

(** [initialize fl ct] is a gui that represents the level in file [fl] and
    the controller [ct]. *)
val initialize : string -> Controller.t -> t

(** [show w] is unit. It has the side-effect of displaying the GUI window. *)
val show : GWindow.window -> unit

(** [message gui str] is unit. It has the side-effect of updating the message
    string displayed on screen. *)
val message : t -> string -> unit