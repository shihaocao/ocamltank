(** 
    Processes user input such as keyboard button presses, mouse position,
    and mouse clicks. 
*)
open Input
open State
open Movable
open Util
open Const

(**[set_player_vel player u] sets the player tank's velocity according to keys 
   pressed in [u]*)
let set_player_vel (player:Movable.tank) u =
  let x0 = match u.a, u.d with
    | true, false -> -1.0
    | false, true -> 1.0
    | _ -> 0.0 in
  let y0 = match u.w, u.s with
    | true, false -> 1.0
    | false, true -> -1.0
    | _ -> 0.0 in
  {
    player with velocity = (player_speed *. x0, player_speed *. y0)
  }

(** [gen_bullet st u] calculates the difference between the player tank
    position and the mouse's position on the screen.  *) 
let gen_bullet st u =
  let player = get_player_tank st.tanks in 
  let target_vec_hat = fdiff u.m_pos player.loc |> unit_vec in 
  let bull_vel = fscale target_vec_hat Const.standard_vel in
  let spawn_loc = fsum player.loc 
      (fscale target_vec_hat (Const.tank_rad +. Const.eps)) in 
  (make_bouncy spawn_loc bull_vel 2)::st.projectiles

(** [generate_palyer_proj player u] spawns a projectile *)
let query_player_shoot st u =
  let player = get_player_tank st.tanks in
  (* 5 is a hard coded min reload time *)
  let shoot = st.cycle_no - player.last_fire_time > Const.standard_reload &&
              u.lmb in 
  let new_projectiles = if shoot then gen_bullet st u else st.projectiles in
  let new_player_tank = if shoot then {player with last_fire_time = st.cycle_no} 
    else player in  
  {
    st with projectiles = new_projectiles;
            tanks = update_tl_player st.tanks new_player_tank
  }

(** [move_player st u] is a new state where the player 
    is moved based on which key strokes are pressed*)
let move_player st (u:Input.user_in_data) =
  let player = get_player_tank st.tanks in
  let enemies = List.filter (fun t -> t.side = Enemy) st.tanks in
  let new_tank_list = (set_player_vel player u)::enemies in
  {
    st with tanks = new_tank_list;
  }

(**[process_u_in st u] sets velocities and spawns things as needed 
   in [st] based on [u] *)
let process_u_in st u = 
  let output = move_player st u in 
  let output = query_player_shoot output u in 
  output