
let rec read_user_code st gr =
  Print.print_grid st gr;
  print_string "\nInput code:\n";
  match read_line () with
  | str -> begin try str |> Eval.parse |> Eval.init_stream 
      with e -> 
        print_endline "\nInterpreting error. Please try again.\n"; 
        read_user_code st gr end

let match_move st = match State.move st with
  | exception e -> st
  | new_st -> new_st

let color_of_int i = match i with
  | 1 -> Grid.Red
  | 2 -> Grid.Green
  | 3 -> Grid.Blue
  | _ -> failwith "Non-defined color"

let rec run_simulation st gr ms =
  Print.print_grid st gr;
  let hd_opt = try Some (Eval.hd ms) 
    with e -> None in
  if State.check_win st gr then print_endline "\nCongratulations. You won!."
  else if hd_opt =  None 
  then print_endline "Your code terminated but you did not win :(\n"
  else 
    let _ = print_string "\nHelpful statement explaining commands q and n\n" in
    let tl = Eval.tl ms in
    match read_line () with
    | "q" -> ()
    | "s" -> () (* stop simulation *)
    | "n" -> begin match hd_opt with
        | Some Eval.M -> run_simulation (match_move st) gr tl
        | Some Eval.R -> run_simulation (State.turn State.Right st) gr tl
        | Some Eval.L -> run_simulation (State.turn State.Left st) gr tl
        | Some Eval.C (c) -> begin 
            run_simulation (State.color (color_of_int c) st) gr tl
          end
        | None -> failwith "Move stream malformed."
      end
    | _ -> print_string "\nInvalid command.\n"; run_simulation st gr ms


let rec init_game () =
  let st_gr fl =
    let gr = fl |> Yojson.Basic.from_file |> Grid.from_json in
    let st = State.init_state gr in 
    run_simulation st gr (read_user_code st gr) in
  match read_line () with
  | exception End_of_file -> ()
  | "1" -> st_gr "json_files/level1.json"
  | "2" -> st_gr "json_files/level2.json"
  | "3" -> st_gr "json_files/example.json"
  | "q" -> ()
  | _-> begin 
      print_string "\nUnrecognized level. Please enter valid command: \n"; 
      init_game () end

let main () =
  print_string "Welcome. Please enter a level number (1,2,3) to play \
                or (q) to quit: \n";
  init_game ()

let () = main ()