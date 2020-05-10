open GMain

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

let img_file x y agent str =
  let ax,ay,dir = agent in
  if x = ax && y = ay then 
    match dir with
    | Model.Grid.N -> "resources/graphics/n_" ^ str
    | Model.Grid.E -> "resources/graphics/e_" ^ str
    | Model.Grid.W -> "resources/graphics/w_" ^ str
    | Model.Grid.S -> "resources/graphics/s_" ^ str
  else "resources/graphics/" ^ str

let update m glist agent =
  let f e = match e with
    | x, y, Model.Grid.Red -> 
      m.(x-1).(y-1)#set_file (img_file x y agent "r.png"); e
    | x, y, Model.Grid.Green -> 
      m.(x-1).(y-1)#set_file (img_file x y agent "g.png"); e
    | x, y, Model.Grid.Blue -> 
      m.(x-1).(y-1)#set_file (img_file x y agent "b.png"); e
    | x, y, Model.Grid.Wall -> 
      m.(x-1).(y-1)#set_file (img_file x y agent "o.png"); e in
  glist |> List.map f |> ignore

let initialize_grid (grid : GPack.grid) =
  let temp = 
    GMisc.image 
      ~file:"resources/graphics/empty.png" 
      ~packing:(grid#attach ~left:0 ~top:0) () in
  let matrix = Array.make_matrix 10 10 temp in
  for i = 0 to 9 do
    for j = 0 to 9 do
      if i = 0 && j = 9 then () else  
        let b = 
          GMisc.image 
            ~file:"resources/graphics/empty.png" 
            ~packing:(grid#attach ~left:i ~top:(9-j)) () in
        matrix.(i).(j) <- b
    done
  done; 
  matrix

let initialize fl ct = 
  GtkMain.Main.init () |> ignore;
  let app = new Gui_class.window () in
  {
    app = app;
    window = app#window;
    grid_matrix = initialize_grid app#agent;
    win_matrix = initialize_grid app#win;
    filename = fl;
    ct = ct;
    program_loaded = false;
    th = None;
    run = false;
    textbox = app#input;
    msg = (let m = app#msg in m#set_editable false; m);
    instr = begin 
      let m = app#instr in
      m#set_editable false;
      ct 
      |> Controller.get_grid 
      |> Model.Grid.get_instructions 
      |> m#buffer#set_text; 
      m end;
    play = app#play;
    next = app#next;
    retry = app#retry;
    compile = app#compile;
    quit = app#quit;
  }

let message gui str =
  gui.msg#buffer#set_text str

let show window =
  window#show ();
  Main.main ()