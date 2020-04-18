open Grid.ml

exception Invalid

type square = {
  x : int;
  y : int;
  current_color: Grid.color;
  obstacle: bool;
}

type agent = {
  x : int;
  y: int;
  current_orien: Grid.orientation;
}

type t = {
  agent: agent;
  current_grid : square list;
  score : int;
}

let init_state g = {
  agent = {x=Grid.get_agent_x g; y=Grid.get_agent_y g; current_orien = Grid.get_agent_orien g;};
  current_grid = Grid.get_start_grid g;
  score = 0;
}

let helper_move st g =
  if st.current_orien = "north" && (st.agent.y > 1) then
    {x=st.agent.x; y=st.agent.y+1; current_orien = st.agent.current_orien;}
  else if st.current_orien = "south" && (st.agent.y < Grid.get_size g) then
    {x=st.agent.x; y=st.agent.y-1; current_orien = st.agent.current_orien;}
  else if st.current_orien = "east" && (st.agent.x < Grid.get_size g) then
    {x=st.agent.x+1; y=st.agent.y; current_orien = st.agent.current_orien;}
  else if st.current_orien = "west" && (st.agent.x > 1) then
    {x=st.agent.x-1; y=st.agent.y; current_orien = st.agent.current_orien;}
  else raise Invalid

let move st g = {
  agent = (helper_move st g);
  current_grid = st.current_grid; (* This is wrong, will change it later*)
  score = st.score; (* not sure if the score changes when the agent moves *)
}

let turn command st = failwith "Unimplemented"

let get_score st = st.score
