
let read_user_code st gr =
  Print.print_grid st gr;
  print_string "\nInput code:\n";
  match read_line () with
  |str -> str |> Eval.parse |> Eval.init_stream
(* catch exception *)

let rec run_simulation st gr ms =
  Print.print_grid st gr;
  let hd = Eval.hd ms in
  let tl = Eval.tl ms in
  print_string "\nHelpful statement explaining commands q and n\n";
  match read_line () with
  | "q" -> ()
  | "s" -> () (* stop simulation *)
  | "n" -> begin match hd with
      |Eval.M -> run_simulation (State.move st) gr tl
      |Eval.R -> run_simulation (State.turn State.Right st) gr tl
      |Eval.L -> run_simulation (State.turn State.Left st) gr tl
      |Eval.C (c) -> run_simulation (State.color Grid.Red st) gr tl (* Pattern match c.*)
    end
  | _ -> print_string "\nInvalid command.\n"; run_simulation st gr ms


let rec init_game () =
  match read_line () with
  | exception End_of_file -> ()
  | "1" -> begin 
      let gr = "example.json" |> Yojson.Basic.from_file |> Grid.from_json in
      let st = State.init_state gr in run_simulation st gr (read_user_code st gr) end
  | "q" -> ()
  | _-> begin print_string "\nUnrecognized level. Please enter valid level: \n"; 
      init_game () end

let main () =
  print_string "Welcome. Please enter level to play: \n";
  init_game ()

let () = main ()