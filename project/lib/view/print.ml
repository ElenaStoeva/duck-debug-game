
(** [string_of_square att] is string "r", "b", "g", or "#" matching with grid 
    attribute [att]. *)
let string_of_square (att : Model.Grid.attribute) = match att with
  | Red -> "1"
  | Green -> "2"
  | Blue -> "3"
  | Wall -> "#"

let rec string_of_row lst = match lst with
  | [] -> ""
  | (_,_,a)::t -> a^" | "^(string_of_row t)

let rec print_grid_aux glst wlst cur n = 
  if cur = 0 then () else
    let row = List.filter (fun (_,y,_) -> y = cur) glst in
    let srow = List.sort (fun (x,_,_) (xx,_,_) -> compare x xx) row in
    let wrow = List.filter (fun (_,y,_) -> y = cur) wlst in
    let wsrow = List.sort (fun (x,_,_) (xx,_,_) -> compare x xx) wrow in
    let hbar1 = String.make (4 * n + 1) '-' in
    let hbar2 = String.make (4 * n + 1) '-' in
    print_string "| ";
    print_string (string_of_row srow);
    print_string "   ";
    print_string "| ";
    print_string (string_of_row wsrow);
    print_string "\n";
    print_string hbar1;
    print_string "    ";
    print_endline hbar2;
    print_grid_aux glst wlst (cur-1) n

let rec win_grid wlst glst acc =
  let win_grid_aux x y = 
    match List.find_opt (fun (xx,yy,_) -> xx = x && yy = y) wlst with
    | Some (_,_,aa) -> (x,y, (string_of_square aa))
    | None -> (x,y,".") 
  in
  match glst with
  | [] -> acc
  | (x,y,_)::t -> (win_grid_aux x y)::(win_grid wlst t acc)

let print_grid st gr =
  print_string (Model.Grid.get_instructions gr);
  print_endline (
    ("\nGrid"^(String.make (4 * (Model.Grid.get_size gr) - 3) '-'))
    ^"    "
    ^("Winning"^(String.make (4 * (Model.Grid.get_size gr) - 6) '-'))
  );
  let ax, ay = st |> Model.State.get_agent |> fun (x,y,_) -> (x,y) in
  let lst = Model.Grid.win_to_list gr in
  let glst = st |> Model.State.to_list in
  let wlst = win_grid lst glst [] in
  let f = fun (x,y,a) -> 
    begin if ax <> x || ay <> y then (x,y,(string_of_square a)) 
      else match Model.State.get_agent st with 
        | (x,y,Model.Grid.N) -> (x,y,"^")
        | (x,y,Model.Grid.E) -> (x,y,">")
        | (x,y,Model.Grid.W) -> (x,y,"<")
        | (x,y,Model.Grid.S) -> (x,y,"v")
    end in
  let glst = st |> Model.State.to_list |> List.map f in
  gr |> Model.Grid.get_size |> fun x -> print_grid_aux glst wlst x x


