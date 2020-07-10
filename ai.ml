(** 
   Representation of AI enemy tanks with algorithm for these tanks
   to move and shoot projectiles on their own.
*)
open Movable
open Interactions
open State
open Util
open Const
open Block 

(** [cap_velocity t accel] returns true when the sum of [accel] and the 
    velocity of [t] is less than or equal to an enemy's maximum speed. *)
let cap_velocity (t : Movable.tank) accel = 
  fsum t.velocity accel |> magn <= max_enemy_speed

(** [randomize t] is a tank with a new, random acceleration added to 
    [t]'s velocity. *)
let rec randomize (t : Movable.tank) : Movable.tank =
  let x_accel = (Random.float 1.0) *. 2.0 |> (-.) 1.0 in
  let y_accel = (Random.float 1.0) *. 2.0 |> (-.) 1.0 in
  let accel_magnitude = max_enemy_accel in
  let accel = fscale ((x_accel, y_accel) |> unit_vec) accel_magnitude in
  if cap_velocity t accel then {t with velocity = fsum t.velocity accel}
  else randomize t

(** [move_all_enemies st] is a state where all enemy tanks in [st] have
    a randomized velocity.

    Requires: [st] is a valid game state.  *)
let move_all_enemies st = 
  let updated_tanks = 
    List.map (fun x -> if x.side = Enemy then randomize x else x) st.tanks in
  {st with tanks = updated_tanks}

(**[clear_los w player enemy] is true iff there's a clear los to player*)
let clear_los (wl:Block.block list) (player:Movable.tank) (enemy:Movable.tank) =
  let target_vec = fdiff player.loc enemy.loc in 
  let total_dist = target_vec |> magn in 
  let target_vec_hat =  target_vec |> unit_vec in 
  let tiny_dt = fscale target_vec_hat 0.01 in 
  let num_query  = total_dist /. Const.query_distance in 
  let probe_coords = 
    List.map (fun x -> (fsum (fscale tiny_dt x) enemy.loc) |> pfloor) 
      (float_range num_query) in
  let simp_probe = List.sort_uniq comp_pair_s probe_coords in
  let wall_locs = List.map (fun x -> pfloor x.coord) wl in 
  let bools = List.map (fun x -> List.mem x wall_locs) simp_probe in 
  let wall_in_way = List.fold_left (fun acc x -> x || acc) false bools in 
  not wall_in_way

(**[can_shoot ccno tank] returns a bool of whether a tank can shoot*)
let can_shoot ccno tank = 
  ccno - tank.last_fire_time > Const.enemy_reload

(**[attempt_shoot wl ccno player enemy] is an option projectile and a tank*)
let attempt_shoot wl ccno (player:Movable.tank)  (enemy:Movable.tank) =
  let fire = can_shoot ccno enemy in 
  if fire && clear_los wl player enemy then 
    let new_tank = {
      enemy with last_fire_time = ccno;
    } in
    let target_vec_hat = fdiff player.loc enemy.loc |> unit_vec in 
    let b_spawn_l = 
      fscale target_vec_hat (Const.tank_rad +. Const.eps) |> fsum enemy.loc in
    let b_vel = fscale target_vec_hat Const.enemy_bullet_vel in 
    (Some (make_bouncy b_spawn_l b_vel 3), new_tank)
  else
    (None, enemy)

(** [attempt_shoot_map w st] is a new game state with the locations
    of the tanks in [w] and [st]. In this new state, enemy tanks attempt to shoot
    a projectile at the player tank. If there is a clear shot to the player tank,
    these projectiles will be generated in the state. 

    Requires: [st] is a valid game state. *)
let attempt_shoot_map w st=
  let wl = w.wall_list in 
  let player = get_player_tank st.tanks in 
  let enemy_tank_list = get_enemy_tanks st.tanks in
  let proj_tank_list = 
    List.map (fun t -> attempt_shoot wl st.cycle_no player t) enemy_tank_list in 
  let (new_projs, new_enemies) = List.split proj_tank_list in
  let new_projs = List.filter_map (fun x -> x) new_projs in 
  {
    st with projectiles = st.projectiles @ new_projs;
            tanks = update_tl_enemies st.tanks new_enemies;
  }

