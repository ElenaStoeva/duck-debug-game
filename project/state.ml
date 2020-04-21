exception Invalid

type direction = Right | Left (* This should come from the interpreter *)

type agent = {
  x : int;
  y: int;
  current_orien: Grid.orientation;
}

type t = {
  agent: agent;
  current_grid : Grid.square list;
}

let init_state g = {
  agent = {x=Grid.get_agent_x g; y=Grid.get_agent_y g; current_orien = Grid.get_agent_orien g;};
  current_grid = Grid.get_start_grid g;
}

let new_position_x st g =
  if st.agent.current_orien = E && (st.agent.x < Grid.get_size g) then
    st.agent.x+1
  else if st.agent.current_orien = W && (st.agent.x > 1) then
    st.agent.x-1
  else raise Invalid

let new_position_y st g =
  if st.agent.current_orien = N && (st.agent.y < 1) then
    st.agent.y+1
  else if st.agent.current_orien = S && (st.agent.y < Grid.get_size g) then
    st.agent.y-1
  else raise Invalid

let move st g = {
  agent = {x=new_position_x st g; y=new_position_y st g; current_orien = st.agent.current_orien; };
  current_grid = st.current_grid;
}

let new_orien direction st =
  match direction with
  | Right -> if st.agent.current_orien = N then Grid.E else W
  | Left -> if st.agent.current_orien = N then W else E

let turn direction st = {
  agent = {x=st.agent.x; y=st.agent.y; current_orien = new_orien direction st};
  current_grid = st.current_grid;
}

let rec helper_color color x y grid acc =
  match grid with
  | [] -> acc
  | h::t -> if (Grid.get_square_x h) = x && (Grid.get_square_y h) = y then 
      helper_color color x y t ((Grid.color_square h color)::acc)
    else helper_color color x y t (h::acc)

let color color st = {
  agent = st.agent;
  current_grid = (helper_color color st.agent.x st.agent.y st.current_grid []);
}

