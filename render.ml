(** 
   Renders all parts of the game (tanks, projectiles, walls, ditches, and
   grid coordinates) using OCaml Graphics. 
*)

open Graphics
open Movable
open State
open Input 
open Const
open Util

(**[start_rend] is of type unit and initializes the rendering*)
let start_rend () = 
  open_graph ":0";
  auto_synchronize false

(**[draw_tank t] is of type unit and draws a rendering of a tank [t] *)
let draw_tank (t:Movable.tank)=
  if t.side = Enemy then set_color red else set_color blue;
  fill_circle (fst t.loc |> int_of_float) (snd t.loc |> int_of_float) 
    ((scale *. 0.4) |> int_of_float)

(**[draw_wall t] is of type unit and draws a rendering of a wall [t] *)
let draw_wall (t:Block.block)=
  if t.kind = Wall then set_color yellow else set_color (rgb 139 69 19);
  let scaled_x = fst t.coord |> int_of_float in
  let scaled_y = snd t.coord |> int_of_float in
  fill_rect scaled_x scaled_y (int_of_float scale * 1) 
    (int_of_float scale * 1)

(**[draw_projectile p] is of type unit and draws a rendering of a projectile
   [p] *)
let draw_projectile (p:Movable.projectile) =
  set_color black;
  set_line_width 2;
  let x = fst p.loc |> int_of_float in
  let y = snd p.loc |> int_of_float in
  let x_vel = fst p.velocity |> int_of_float in
  let y_vel = snd p.velocity |> int_of_float in
  moveto x y;
  lineto (x + x_vel) (y + y_vel);
  set_line_width 1

(**[draw_projectiles pl] is of type unit list and draws all projectiles in 
   [pl] *)
let draw_projectiles (pl : Movable.projectile list) =
  List.map draw_projectile pl

(**[draw_walls tl] is of type unit list and draws all walls *)
let draw_walls (tl:Block.block list) =
  List.map draw_wall tl

(**[draw_tanks tl] is of type unit list and draws all tanks *)
let draw_tanks (tl:Movable.tank list) =
  List.map draw_tank tl

(**[remap_tank t] is of type tank and scales the tank accordingly to the map *)
let remap_tank (t:Movable.tank) =
  let new_loc = match t.loc with
    | (x, y) -> (scale *. x, scale *. y) in
  {t with loc = new_loc}

(**[remap_proj t] is of type projectile and scales the proojectile accordingly 
   to the map *)
let remap_proj (t:Movable.projectile) =
  let new_loc = match t.loc with
    | (x, y) -> (scale *. x, scale *. y) in
  {t with loc = new_loc}

(**[remap_bloock t] is of type block and scales the block [t] accordingly to the 
   map *)
let remap_block (t:Block.block) =
  let new_loc = match t.coord with
    | (x, y) -> (scale *. (x -. 0.5), scale *. (y -. 0.5)) in
  {t with coord = new_loc}

(** [remap_coords_state st] takes in a state [st], multiplies grid coords by 
    scale factor and draws them, returning another state*)
let remap_coords_state (st:State.state) =
  {
    st with tanks = List.map remap_tank st.tanks;
            projectiles = List.map remap_proj st.projectiles
  }

(** [remap_coords_world w] takes in a world [w], multiplies grid coords by scale 
    factor and draws them, returning another world*)
let remap_coords_world (w:State.world) =
  {
    wall_list = List.map remap_block w.wall_list;
    ditch_list = List.map remap_block w.ditch_list;
  }

(**[draw_grid] is of type unit and draws grid lines*)
let draw_grid () =

  set_color yellow;

  let d2 = float_range 30.0 in
  let d_scaled = List.map (fun x -> x *. scale |> int_of_float) d2 in 

  let draw_horiz y_val = 
    moveto 0 y_val;
    rlineto 1000 0;
  in
  let draw_vert x_val =
    moveto x_val 0;
    rlineto 0 1000;
  in
  let _ = List.map draw_horiz d_scaled in 
  let _ = List.map draw_vert d_scaled in 
  ()

(**[render_frame w st] is of type unit and renders the entire frame of view*)
let render_frame (w:State.world) (st:State.state) =
  let () = clear_graph () in
  let remapped_state = remap_coords_state st in 
  let remapped_world = remap_coords_world w in

  let _ = draw_grid () in

  let _ = draw_tanks remapped_state.tanks in
  let _ = draw_walls remapped_world.wall_list in
  let _ = draw_walls remapped_world.ditch_list in
  let _ = draw_projectiles remapped_state.projectiles in

  let () = synchronize () in ()