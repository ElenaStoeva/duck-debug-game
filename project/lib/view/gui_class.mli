(** [window] is a Gbuilder class that generates the necessary GUI components
    using [resources/gui.glade]. Each method returns the corresponding
    GUI object with the identifier with the method name. *)

class window : ?translation_domain:string -> unit ->
  object
    val agent : GPack.grid
    val buttongrid : GPack.box
    val compile : GButton.button
    val hor : GPack.box
    val input : GEdit.entry
    val leftv : GPack.box
    val next : GButton.button
    val play : GButton.button
    val quit : GButton.button
    val retry : GButton.button
    val rightv : GPack.box
    val toplevel : GWindow.window
    val win : GPack.grid
    val window : GWindow.window
    val msg : GText.view
    val instr : GText.view
    method agent : GPack.grid
    method buttongrid : GPack.box
    method compile : GButton.button
    method hor : GPack.box
    method input : GEdit.entry
    method leftv : GPack.box
    method next : GButton.button
    method play : GButton.button
    method quit : GButton.button
    method reparent : GObj.widget -> unit
    method retry : GButton.button
    method rightv : GPack.box
    method toplevel : GWindow.window
    method win : GPack.grid
    method window : GWindow.window
    method msg : GText.view
    method instr : GText.view
  end