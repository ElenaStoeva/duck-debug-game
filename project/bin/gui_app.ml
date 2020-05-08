open GMain

let locale = GtkMain.Main.init ()

class window ?translation_domain () =
  let builder = GBuilder.builder ?translation_domain () in
  let _ = builder#add_objects_from_file "resources/gui.glade" ["window"] in
  object
    val toplevel =
      new GWindow.window (GtkWindow.Window.cast (builder#get_object "window"))
    method toplevel = toplevel
    val window =
      new GWindow.window (GtkWindow.Window.cast (builder#get_object "window"))
    method window = window
    val hor =
      new GPack.box (GtkPack.Box.cast (builder#get_object "hor"))
    method hor = hor
    val leftv =
      new GPack.box (GtkPack.Box.cast (builder#get_object "leftv"))
    method leftv = leftv
    val agent =
      new GPack.grid (GtkPack.Grid.cast (builder#get_object "agent"))
    method agent = agent
    val input =
      new GEdit.entry (GtkEdit.Entry.cast (builder#get_object "input"))
    method input = input
    val rightv =
      new GPack.box (GtkPack.Box.cast (builder#get_object "rightv"))
    method rightv = rightv
    val win =
      new GPack.grid (GtkPack.Grid.cast (builder#get_object "win"))
    method win = win
    val buttongrid =
      new GPack.box (GtkPack.Box.cast (builder#get_object "buttongrid"))
    method buttongrid = buttongrid
    val compile =
      new GButton.button (GtkButton.Button.cast (builder#get_object "compile"))
    method compile = compile
    val next =
      new GButton.button (GtkButton.Button.cast (builder#get_object "next"))
    method next = next
    val play =
      new GButton.button (GtkButton.Button.cast (builder#get_object "play"))
    method play = play
    val retry =
      new GButton.button (GtkButton.Button.cast (builder#get_object "retry"))
    method retry = retry
    val quit =
      new GButton.button (GtkButton.Button.cast (builder#get_object "quit"))
    method quit = quit
    method reparent parent =
      hor#misc#reparent parent;
      toplevel#destroy ()
  end

let rec win_grid 
    (wlst : (int * int * Model.Grid.attribute) list)
    (glst : (int * int * Model.Grid.attribute) list)
    (acc : (int * int * string) list) =
  let win_grid_aux x y = 
    match List.find_opt (fun (xx,yy,_) -> xx = x && yy = y) wlst with
    | Some (_,_,Model.Grid.Red) -> (x,y, "resources/graphics/r.png")
    | Some (_,_,Model.Grid.Green) -> (x,y, "resources/graphics/g.png")
    | Some (_,_,Model.Grid.Blue) -> (x,y, "resources/graphics/b.png")
    | Some (_,_,Model.Grid.Wall) -> (x,y, "resources/graphics/o.png")
    | None -> (x,y,"resources/graphics/empty.png") 
  in
  match glst with
  | [] -> acc
  | (x,y,_)::t -> (win_grid_aux x y)::(win_grid wlst t acc)

let draw_win m gr st =
  let lst = Model.Grid.win_to_list gr in
  let glst = st |> Model.State.to_list in
  let wlst = win_grid lst glst [] in
  List.map (fun (x,y,fl) -> m.(x-1).(y-1)#set_file fl; (x,y,fl)) wlst  
  |> ignore

let update m glist agent =
  let f e = match e with
    | x, y, Model.Grid.Red -> m.(x-1).(y-1)#set_file "resources/graphics/r.png"; e
    | x, y, Model.Grid.Green -> m.(x-1).(y-1)#set_file "resources/graphics/g.png"; e
    | x, y, Model.Grid.Blue -> m.(x-1).(y-1)#set_file "resources/graphics/b.png"; e
    | x, y, Model.Grid.Wall -> m.(x-1).(y-1)#set_file "resources/graphics/o.png"; e in
  let _ = glist |> List.map f in
  let g = match agent with
    | (x,y,Model.Grid.N) -> m.(x-1).(y-1)#set_file "resources/graphics/n.png"
    | (x,y,Model.Grid.E) -> m.(x-1).(y-1)#set_file "resources/graphics/e.png"
    | (x,y,Model.Grid.W) -> m.(x-1).(y-1)#set_file "resources/graphics/w.png"
    | (x,y,Model.Grid.S) -> m.(x-1).(y-1)#set_file "resources/graphics/s.png" in g; ()


let main () =
  let app = new window () in
  let window = app#window in
  window#connect#destroy ~callback:Main.quit |> ignore;
  let quit = app#quit in
  quit#connect#clicked ~callback:Main.quit |> ignore;

  (* initialize game grid matrix *)
  let grid = app#agent in
  let temp = GMisc.image ~file:"resources/graphics/empty.png" ~packing:(grid#attach ~left:0 ~top:0) () in
  let matrix = Array.make_matrix 10 10 temp in
  matrix.(0).(0) <- temp;

  for i = 0 to 9 do
    for j = 0 to 9 do
      if i = 0 && j = 0 then () else  
        (*  let mes = (string_of_int i) ^ "," ^ (string_of_int j) in
            let b = GButton.button ~label:mes ~packing:(grid#attach ~left:i ~top:j) () in *)
        let b = GMisc.image ~file:"resources/graphics/empty.png" ~packing:(grid#attach ~left:i ~top:j) () in
        (* b#connect#clicked ~callback: (fun () -> prerr_endline mes) |> ignore; *)
        matrix.(i).(j) <- b
    done
  done;

  (* initialize winning grid matrix *)
  let wgrid = app#win in
  let temp = GMisc.image ~file:"resources/graphics/empty.png" ~packing:(wgrid#attach ~left:0 ~top:0) () in
  let wmatrix = Array.make_matrix 10 10 temp in
  wmatrix.(0).(0) <- temp;

  for i = 0 to 9 do
    for j = 0 to 9 do
      if i = 0 && j = 0 then () else  
        (*  let mes = (string_of_int i) ^ "," ^ (string_of_int j) in
            let b = GButton.button ~label:mes ~packing:(grid#attach ~left:i ~top:j) () in *)
        let b = GMisc.image ~file:"resources/graphics/empty.png" ~packing:(wgrid#attach ~left:i ~top:j) () in
        (* b#connect#clicked ~callback: (fun () -> prerr_endline mes) |> ignore; *)
        wmatrix.(i).(j) <- b
    done
  done;

  (* initialize controller *)
  let program_loaded = ref false in
  let prog_str = "" in
  let filename = ref "resources/json_files/level4.json" in
  let ct = ref (Controller.initialize !filename prog_str) in

  let handle_next ct () =
    if !program_loaded then 
      let _ = match Controller.next !ct with
        | Gameover m -> prerr_endline ("try again "^m)
        | Winning i -> prerr_endline ("you won"^(string_of_int i))
        | Next (ct',m) -> prerr_endline m; ct := ct' in
      let st = Controller.get_state !ct in
      update matrix (st |> Model.State.to_list) (st |> Model.State.get_agent)
    else prerr_endline "you need to compile" in

  let next = app#next in
  next#connect#clicked ~callback:(handle_next ct) |> ignore; 

  let textbox = app#input in
  let run = ref false in
  let th = ref (Some (Thread.self ())) in

  let handle_retry ct () =
    if !program_loaded || !run then begin
      (* match !th with None -> () | Some x -> Thread.kill x; *)
      run := false;
      program_loaded := false;
      textbox#set_editable true;
      ct := Controller.initialize !filename (Controller.get_program !ct); 
      let st = Controller.get_state !ct in
      update matrix (st |> Model.State.to_list) (st |> Model.State.get_agent)
    end
    else prerr_endline "game already running" in

  let retry = app#retry in
  retry#connect#clicked ~callback:(handle_retry ct) |> ignore;

  let handle_compile ct () =
    if !program_loaded = false then begin
      program_loaded := true;
      ct := Controller.initialize !filename (textbox#text);
      textbox#set_editable false;
      let st = Controller.get_state !ct in
      update matrix (st |> Model.State.to_list) (st |> Model.State.get_agent)
    end
    else prerr_endline "game already running" in

  let compile = app#compile in
  compile#connect#clicked ~callback:(handle_compile ct) |> ignore;

  let handle_play ct () =
    if !run = true then prerr_endline "simulation already running"
    else if !program_loaded = false then prerr_endline "compile first"
    else begin run := true;
      textbox#set_editable false;
      let t = Thread.create (
          let rec f () = if !run = false then prerr_endline "thread terminated"
            else begin
              let b = match Controller.next !ct with
                | Gameover m -> prerr_endline ("try again "^m); false
                | Winning i -> prerr_endline ("score "^(string_of_int i)); false
                | Next (ct',m) -> prerr_endline m; ct := ct'; true in
              let st = Controller.get_state !ct in
              update matrix (st |> Model.State.to_list) 
                (st |> Model.State.get_agent);
              prerr_endline "thread loop";
              Thread.delay 0.5; 
              if b then f () 
              else run := false; prerr_endline "thread terminated"
            end in f) () in
      th := Some t 
    end in


  let play = app#play in
  play#connect#clicked ~callback:(handle_play ct) |> ignore;

  update matrix (Controller.get_state !ct |> Model.State.to_list) 
    (Controller.get_state !ct |> Model.State.get_agent);
  draw_win wmatrix (Controller.get_grid !ct) (Controller.get_state !ct);
  window#show ();
  Main.main ()