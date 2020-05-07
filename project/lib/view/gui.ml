open Tsdl
open Bogue
module W = Widget
module L = Layout

let main () =

  let input = W.text_input ~max_size:200 ~prompt:"Input code:" () in
  let label = W.label ~size:40 "Your code is: " in
  let layout = L.tower [L.resident ~w:400 input;
                        L.resident ~w:400 ~h:200 label] in

  (* type action = W.t -> W.t -> Tsdl.Sdl.event -> unit
     An action is a function with three parameters w1 w2 ev, where w1 is 
     the source widget, w2 the target widget, and ev the event (Trigger.t) that 
     triggered the action. *)
  let action ti l _ = 
    let text = W.get_text ti in
    W.set_text l ("Your code is: " ^ text) in

  (* [connect source target action triggers] creates a connection from the 
     source widget to the target widget, but does not register it (this may 
     change in the future...). Once it is registered (either by Main.make or 
     Widget.add_connection), and assuming that the layout containing the source 
     widget has focus, then when an event ev matches one of the triggers list, 
     the action is executed with arguments source target ev.*)
  let c = W.connect input label action Sdl.Event.[text_input; key_down] in

  let board = Bogue.make [c] [layout] in
  Bogue.run board;;

let () = main ();
  Bogue.quit ();;