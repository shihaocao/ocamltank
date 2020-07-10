(** 
   Representation of a json file as a game state and a map. 
*)
open Yojson.Basic.Util
open Movable
open Block

(** the type t of read_json *)
type t = {
  tank_list : Movable.tank list;
  wall_list : Block.block list;
  ditch_list : Block.block list;
}

(** [read_side str] returns the team corresponding with [str]. 
    If [str] is Self, it will return Self. If [str] is Enemy, it will return 
    Enemy. Otherwise, it will fail with "not valid tank type". *)
let read_side str = match str with
  | "Self" -> Self
  | "Enemy" -> Enemy
  | _ -> failwith "not valid tank type"

(** [lst_to_tuple lst] returns a pair of [lst] only has 2 elements.
    Otherwise, it fails with "invalid list." *)
let lst_to_tuple lst = match lst with
  | h :: s :: [] -> (h, s)  
  | _ -> failwith "invalid list"

(** [tank_of_json json] is a tank

    Requires: [json] is a valid JSON map representation. *)
let tank_of_json json = {
  loc = json 
        |> member "loc" 
        |> to_list
        |> List.map (to_float) 
        |> lst_to_tuple;
  past_loc = json 
             |> member "loc" 
             |> to_list 
             |> List.map (to_float) 
             |> lst_to_tuple;
  velocity = (0.0, 0.0);
  health = json 
           |> member "health" 
           |> to_int;
  last_fire_time = 0;
  side = json 
         |> member "side" 
         |> to_string 
         |> read_side;
}

(** [read_block_kind str] returns the block type corresponding with [str]. 
    If [str] is Wall, it will return Wall. If [str] is Ditch, it will return 
    Ditch. Otherwise, it will fail with "not valid tank type".*)
let read_block_kind str = match str with
  | "Wall" -> Wall
  | "Ditch" -> Ditch
  | _ -> failwith "invalid block kind"

(** [wall_of_json json] is a wall

    Requires: [json] is a valid JSON map representation. *)
let wall_of_json json = {
  id = json |> member "name" |> to_string;
  kind = Wall;
  width = json |> member "width" |> to_float;
  coord = json 
          |> member "coord" 
          |> to_list 
          |> List.map (to_float) 
          |> lst_to_tuple;
}

(** [ditch_of_json json] is a ditch

    Requires: [json] is a valid JSON map representation. *)
let ditch_of_json json = {
  id = json 
       |> member "name" 
       |> to_string;
  kind = Ditch;
  width = json 
          |> member "width" 
          |> to_float;
  coord = json 
          |> member "coord" 
          |> to_list 
          |> List.map (to_float) 
          |> lst_to_tuple;
}

(** [from_json json] is a read_json object

    Requires: [json] is a valid JSON map representation. *)
let from_json json = {
  tank_list = json 
              |> member "tanks" 
              |> to_list 
              |> List.map tank_of_json;
  wall_list = json 
              |> member "walls"
              |> to_list 
              |> List.map wall_of_json;
  ditch_list = json 
               |> member "ditches" 
               |> to_list 
               |> List.map ditch_of_json;
}