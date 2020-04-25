

let string_of_square (att:Grid.attribute) = match att with
  | Red -> "0"
  | Blue -> "1"
  | Green -> "2"
  | Wall -> "#"

let rec string_of_row st lst = match lst with
  | [] -> "|"
  | (x,y,a)::t -> "|"^(string_of_square a)^"|"^(string_of_row st t)

let rec print_grid_aux st cur = 
  if cur = 0 then () else
  let grid = State.to_list st in
  let row = List.filter (fun (_,y,_) -> y = cur) grid in
  let srow = List.sort (fun (x,_,_) (xx,_,_) -> compare x xx) row in
  print_string (string_of_row st srow);
  print_string "\n----------------------------\n";
  print_grid_aux st (cur-1)

let print_grid st gr =
  gr |> Grid.get_size |> print_grid_aux st
