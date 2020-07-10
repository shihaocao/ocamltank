(**
    Loading of game state and world.
*)
open Read_json
open Block
open State

(** [init_state map] is the initial state of the game when playing [map]. 
    In that state, the player's score is 100,000, and there are no live
    projectiles in the state. *)
let init_state map = {
  sys_time = 0.0;
  cycle_no = 0; 
  score = 100 * 100 * 10;
  tanks = map.tank_list;
  projectiles = [];
  win_cond = Playing;
}

(** [init_world map] is the initial state of the map when playing [map]. 
    In this world, the map's wall list is initialized with the wall list in 
    [map] and its ditch list is initialized with the ditch list in [map]. *)
let init_world (map:Read_json.t) = {
  wall_list = map.wall_list;
  ditch_list = map.ditch_list;
}

(** [json_file_to_map] reads in a json file *)
let json_file_to_map f =
  try let json = f |> Yojson.Basic.from_file in
    json |> Read_json.from_json
  with e ->      
    ANSITerminal.(print_string [red]
                    "\n\nInvalid Map Name. Please try again.\n");
    Stdlib.exit 0