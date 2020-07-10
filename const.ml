(**
    A set of constants to be referenced by multiple modules.
*)

(** Velocity of player projectiles*)
let standard_vel = 0.035
(** Velocity of enemy projectiles*)
let enemy_bullet_vel = 0.04
(** Radius/size of all tanks*)
let tank_rad = 0.4
(** Reload timer for players*)
let standard_reload = 50
(** Reload timers for enemies*)
let enemy_reload = 60
(** Speed of player tanks*)
let player_speed = 0.1
(** Upper boundary for enemy tank speed*)
let max_enemy_speed = 0.05
(** Upper boundary for enemey tank accelerations*)
let max_enemy_accel = 0.04
(** Queried distance*)
let query_distance = 0.01
(** Scaling factor*)
let scale = 20.0

(** eps, which stands for epsilon, is a small float*)
let eps = 0.1
(** Time per cycle*)
let cycle_time = 0.008
(** Score reduction for death*)
let on_death_subtraction = 100*100*5