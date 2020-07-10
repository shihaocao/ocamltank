(** 
   Representation of movable in-game objects.
*)

open Util

(**type [team] is a variant of either Self or Enemy, describing each tank *)
type team = Self | Enemy

(**type [proj_species] is a variant of either Bouncy or Bullet, describing each 
   projectile *)
type proj_species = Bouncy | Bullet

(**type [tank] is a record of a tank's location, immediate past location, 
   velocity, health, last time it fired a projectile, and its team*)
type tank = {
  loc : float * float;
  past_loc : float * float;
  velocity : float * float;
  health : int;
  last_fire_time : int;
  side : team;
}

(**type [projectile] is a record of a projectile's location, immediate past 
   location, velocity, health, and the weapon species. *)
type projectile = {
  loc : float * float;
  past_loc : float * float;
  velocity : float * float;
  health : int;
  weap_species: proj_species
}

(** [make_bullet l v health] is a standard projectile *)
let make_bullet l v health = {
  loc = l;
  past_loc = fdiff l v;
  velocity = v;
  health = health;
  weap_species = Bullet;
}
(** [make_bouncy l v health] is a Bouncy projectile *)
let make_bouncy l v health = {
  loc = l;
  past_loc = fdiff l v;
  velocity = v;
  health = health;
  weap_species = Bouncy;
}

(** [is_dead t] returns whether or not if a movable [t] is dead *)
let is_dead t =
  t.health = 0

(** [stop_tank t] returns a non-moving tank [t] *)
let stop_tank t =
  {t with velocity = (0.0, 0.0)}

(** [grid_loc (x, y)] is a tuple represting the grid location of a [(x, y)]. *)
let grid_loc (x, y) =
  (x |> int_of_float, y |> int_of_float)

(** [tuple_to_string t] is a tuple [t] represented as a string *)
let tuple_to_string (t : float * float) = 
  "(" ^ (fst t |> string_of_float) ^ ", " ^ (snd t |> string_of_float) ^ ")"

(** [tank_info t] prints the location, velocity, and health of a tank [t] *)
let tank_info (t : tank) = 
  print_string ("Current location: " ^ (tuple_to_string t.loc));
  print_string (", Velocity: " ^ (tuple_to_string t.velocity));
  print_string (", Health: " ^ (string_of_int t.health))

(** [proj_info t] prints the location, velocity, and health of a 
    projectile [p] *)
let proj_info (p : projectile) = 
  print_string ("Current location: " ^ (tuple_to_string p.loc));
  print_string (", Velocity: " ^ (tuple_to_string p.velocity));
  print_string (", Health: " ^ (string_of_int p.health ^ "\n"))