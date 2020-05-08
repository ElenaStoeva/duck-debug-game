open Model
open Interpreter

exception InterpreterError of string

type t = {
  state : State.t;
  grid : Grid.t;
  program : string;
  stream : Eval.move_stream;
}

type result = Winning of int | Gameover of string | Next of t * string

let interpret_program prog_str =
  try prog_str 
      |> Eval.parse 
      |> Check.check_ast 
      |> Eval.init_stream 
  with _ -> raise (InterpreterError "Cannot compile. Check your code!")

let initialize filename prog_str =
  let gr = filename |> Yojson.Basic.from_file |> Grid.from_json in
  let st = State.init_state gr in
  let ms = interpret_program prog_str in
  {
    state = st;
    grid = gr;
    program = prog_str;
    stream = ms;
  }

let reset_prog t prog_str = 
  { t with
    state = State.init_state t.grid;
    program = prog_str;
    stream =  interpret_program prog_str;
  }

let get_state t = t.state

let get_program t = t.program

let get_stream t = t.stream

let get_grid t = t.grid

(** [match_move st] is the new game state after agent has moved, and [st] if 
    moving agent raises exception. *)
let match_move st = match State.move st with
  | exception _ -> st, "the agent cannot move off the grid"
  | new_st -> new_st, "the agent moved"

(** [color_of_int i] is the grid attribute corresponding to [i]. 
    Raises: [Failure] if [i] does not match valid color. *)
let color_of_int i = match i with
  | 1 -> Grid.Red
  | 2 -> Grid.Green
  | 3 -> Grid.Blue
  | _ -> failwith "\nNon-defined color. Color command ignored.\n"

(** [match_prim st m] is the state after primitive move [m] is applied.
    Requires that [m] is not [None]. *)
let match_prim st m = match m with
  | Some Eval.M -> match_move st
  | Some Eval.R -> (State.turn State.Right st), "the agent turned right"
  | Some Eval.L -> (State.turn State.Left st), "the agent turned left"
  | Some Eval.C (c) -> (State.color (color_of_int c) st), "the agent colored sq"
  | None -> failwith "Move stream malformed."

let next t = 
  let hd_opt = try Some (Eval.hd t.stream) 
    with _ -> None in
  if State.check_win t.state t.grid then Winning (Grid.get_score t.grid)
  else if (State.get_steps t.state) = 0 then Gameover ("step count 0")
  else if hd_opt =  None then Gameover ("program terminated")
  else 
    let tl = Eval.tl t.stream in
    let st', m = match_prim t.state hd_opt in
    Next ({ t with state = st'; stream = tl}, m)