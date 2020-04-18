exception Invalid

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


let helper_move st g =
  if st.agent.current_orien = N && (st.agent.y < 1) then
    {x=st.agent.x; y=st.agent.y+1; current_orien = st.agent.current_orien;}
  else if st.agent.current_orien = S && (st.agent.y < Grid.get_size g) then
    {x=st.agent.x; y=st.agent.y-1; current_orien = st.agent.current_orien;}
  else if st.agent.current_orien = E && (st.agent.x < Grid.get_size g) then
    {x=st.agent.x+1; y=st.agent.y; current_orien = st.agent.current_orien;}
  else if st.agent.current_orien = W && (st.agent.x > 1) then
    {x=st.agent.x-1; y=st.agent.y; current_orien = st.agent.current_orien;}
  else raise Invalid

let move st g = {
  agent = (helper_move st g); (* Will simplify this *)
  current_grid = st.current_grid;
}

let turn command st = failwith "Unimplemented"

let color command st = failwith "Unimplemented"

