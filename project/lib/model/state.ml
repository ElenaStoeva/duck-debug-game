exception Invalid_move

exception Wall_exception

type direction = Right | Left

type agent = {
  x : int;
  y : int;
  current_orien : Grid.orientation;
}

type t = {
  agent : agent;
  current_grid : Grid.square list;
  size : int;
  steps_left : int;
}

let init_state g = {
  agent = { 
    x = Grid.get_agent_x g;
    y = Grid.get_agent_y g;
    current_orien = Grid.get_agent_orien g 
  };
  current_grid = Grid.get_start_grid g;
  size = Grid.get_size g;
  steps_left = Grid.get_max_steps g;
}

(** [new_position st] is an [(x,y)] pair where [x] and [y] represent the new
    coordinates of the agent after the movement in state [st].
    Raises [Invalid] if the move is not permitted. *)
let new_position st =
  let x = st.agent.x in
  let y = st.agent.y in
  match st.agent.current_orien with
  | N -> if y = st.size then raise Invalid_move
    else if (Grid.get_att_from_coord st.current_grid x (y+1))= Wall then raise Wall_exception
    else (x,y+1)
  | E -> if x = st.size then raise (Invalid_move)
    else if (Grid.get_att_from_coord st.current_grid (x+1) y) = Wall then raise Wall_exception
    else (x+1,y)
  | W -> if x = 1 then raise (Invalid_move )
    else if (Grid.get_att_from_coord st.current_grid (x-1) y) = Wall then raise Wall_exception
    else (x-1,y) 
  | S -> if y = 1  then raise (Invalid_move)
    else if (Grid.get_att_from_coord st.current_grid x (y-1)) = Wall then raise Wall_exception
    else (x,y-1)

let move st = 
  let x,y = new_position st in
  { st with
    agent = { st.agent with
              x = x;
              y = y;
            };
    steps_left = st.steps_left - 1
  }

(** [new_orien d st] is the orientation of the agent after the turn in 
    direction [d] is applied in state [st]. *)
let new_orien direction st =
  match direction with
  | Right -> begin 
      match st.agent.current_orien with
      | N -> Grid.E
      | E -> Grid.S
      | W -> Grid.N
      | S -> Grid.W
    end
  | Left -> begin 
      match st.agent.current_orien with
      | N -> Grid.W
      | E -> Grid.N
      | W -> Grid.S
      | S -> Grid.E
    end

let turn direction st = { 
  st with
  agent = { st.agent with
            current_orien = new_orien direction st
          };
  steps_left = st.steps_left - 1
}

(** [helper_color c x y g a] is a grid square list where the square in [x],[y]
    coordinates is updated to have attribute [c] in grid [g]. *)
let rec helper_color color x y grid acc =
  match grid with
  | [] -> acc
  | h::t -> begin
      if (Grid.get_square_x h) = x && (Grid.get_square_y h) = y then 
        helper_color color x y t ((Grid.update_square_att h color)::acc)
      else helper_color color x y t (h::acc)
    end

let color_square x y cl st = { 
  st with
  current_grid = helper_color cl x y st.current_grid [];
}

let color cl st = {
  st with
  current_grid = helper_color cl st.agent.x st.agent.y st.current_grid [];
  steps_left = st.steps_left - 1
}


(** [helper_list gl ac] is the list representation of [gl] added to list [ac].*)
let rec helper_list (glst : Grid.square list) acc = match glst with
  | [] -> acc
  | {square_x=x; square_y=y; attribute=a}::t -> helper_list t ((x,y,a)::acc)

let to_list st = helper_list st.current_grid []


(**[helper_check_win] is true if each square in [winning_grid] is in the 
   list [current_grid] *)
let rec helper_check_win winning_grid current_grid =
  match winning_grid with
  | [] -> true
  | h::t -> if not (List.mem h current_grid) then false
    else helper_check_win t current_grid

let check_win st g = helper_check_win (Grid.get_winning_grid g) st.current_grid

let get_agent st = st.agent.x, st.agent.y, st.agent.current_orien

let get_current_color st = Grid.get_att_from_coord st.current_grid st.agent.x st.agent.y

let get_steps st = st.steps_left

