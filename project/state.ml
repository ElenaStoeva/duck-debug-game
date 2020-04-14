type square_id = (int * int)
type color = string
type orientation = N|E|S|W

type square = square_id * color


type agent = {
  current_square: square_id;
  current_orientation: orientation
}

type t ={
  grid: square list;
  agent: agent;
  score: int
}


let init_grid n = failwith "Unimplemented" (*This depends on the JSON files which wile store information about the grid *)


let init_agent id = {
  current_square = id;
  current_orientation = N; (*Here I assumed that the agent starts facing north, we have to discuss this *)
}

let init_state id grid = {
  grid = grid; (*The init_grid function should go here once we implement it *)
  agent = init_agent id;
  score = 0;
}

let move id st = failwith "Unimplemented" (*To do this I need more information about the commands *)
