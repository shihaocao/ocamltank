(**
   Interaction of the in-game objects with one another.
*)
open Block
open Movable
open State
open Util
open Const

(**[move_tank tanks walls] takes in a list of tanks and walls and moves the
   tanks according to their velocities. Set the tanks' velocities to 0.*)
let rec move_tank (tanks:Movable.tank list) = 
  match tanks with  
  | [] -> []
  | h::t -> let new_loc = 
              (fst h.velocity +. fst h.loc, snd h.velocity +. snd h.loc) in
    {h with loc = new_loc; past_loc = h.loc}::move_tank t 

(**[move_projs projs walls] is a list of active projectiles with updated 
   locations. If the proj's new location is inside a wall then remove it, but if 
   it is  *)
let rec move_projs (projs:Movable.projectile list) = 
  match projs with  
  | [] -> []
  | h::t -> let new_loc = 
              (fst h.velocity +. fst h.loc, snd h.velocity +. snd h.loc) in
    {h with loc = new_loc; past_loc = h.loc}::move_projs t

(**[wall_detect coords walls] takes in a entity's coordinates as a tuple and a
   list of walls and returns back a boolean of whether the entity is in a wall*)
let rec wall_detect coords walls= 
  (fst walls.coord -. 0.5) <= (fst coords) && 
  (fst walls.coord +. 0.5) >= fst coords && 
  (snd walls.coord -. 0.5) <= snd coords && 
  (snd walls.coord +. 0.5) >= snd coords 

(**[check_grid coordA coordB] sees if 2 coordinates are less than 2 grid pos 
   away in both x and y directions *)
let check_grid coordA coordB = 
  Float.to_int (fst coordA)|> (-) (Float.to_int (fst coordB)) |> Int.abs < 2
  && Float.to_int (snd coordA)|> (-) (Float.to_int (snd coordB)) |> Int.abs < 2

(**[edge_touch_tank tank corn2 corn1] returns a bool of whether a tank is 
   touching any edge on a wall*)
let edge_touch_tank (tank:Movable.tank) corn2 corn1=
  let tank_radius = 0.4 in
  let ax = fst corn2 -. fst tank.loc in 
  let ay = snd corn2 -. snd tank.loc in 
  let bx = fst corn1 -. fst tank.loc in 
  let by = snd corn1 -. snd tank.loc in
  let a = Float.pow (bx -. ax) 2.0 +. Float.pow (by -. ay) 2.0 in
  let b = 2.0 *. (ax *. (bx -. ax) +. ay *. (by -. ay)) in
  let c = Float.pow ax 2.0 +. Float.pow ay 2.0 -. Float.pow tank_radius 2.0 in 
  let disc = Float.pow b 2.0 -. 4.0 *. a *. c in 
  let sqrt_disc = Float.sqrt disc in 
  let t1 = (Float.neg b +. sqrt_disc) /. (2.0 *. a) in
  let t2 = (Float.neg b -. sqrt_disc) /. (2.0 *. a) in
  disc > 0.0 && ((0.0 < t1 && t1 < 1.0) || (0.0 < t2 && t2 < 1.0)) 

(**[tank_touch_wall wall tank] returns a bool as to whether a tank is touching
   a specific wall*)
let tank_touch_wall wall (tank:Movable.tank)=
  let half_wall_width = 0.5 in
  let tank_radius = 0.4 in
  let corn1=(fst wall.coord-.half_wall_width,snd wall.coord-.half_wall_width) in
  let corn2=(fst wall.coord+.half_wall_width,snd wall.coord-.half_wall_width) in
  let corn3=(fst wall.coord+.half_wall_width,snd wall.coord+.half_wall_width) in
  let corn4=(fst wall.coord-.half_wall_width,snd wall.coord+.half_wall_width) in
  get_distance_from tank.loc corn1 < tank_radius || 
  get_distance_from tank.loc corn2 < tank_radius || 
  get_distance_from tank.loc corn3 < tank_radius ||
  get_distance_from tank.loc corn4 < tank_radius || 
  edge_touch_tank tank corn4 corn1 || edge_touch_tank tank corn2 corn1 ||
  edge_touch_tank tank corn3 corn2 || edge_touch_tank tank corn3 corn4

(**[tank_touch_tank tanks tank] returns a bool of whether a tank touches another
   tank*)
let rec tank_touch_tank (tanks: Movable.tank list) (tank: Movable.tank)=
  match tanks with
  | [] -> false
  | h::t -> if h.loc <> tank.loc && (get_distance_from h.loc tank.loc) < 0.8 
    then true else tank_touch_tank t tank

(**[tank_phys_engine tank walls] returns a tank with values either unchanged or 
   changed depending on if the tank touched a wall*)
let rec tank_phys_engine (tanks:Movable.tank list) (tank:Movable.tank) walls
  : Movable.tank =
  if tank_touch_tank tanks tank then {tank with loc=tank.past_loc;} else
    match walls with
    | [] -> tank
    | h::t ->  if (check_grid h.coord tank.loc && tank_touch_wall h tank) 
      then {tank with loc=tank.past_loc;}
      else tank_phys_engine tanks tank t

(**[edge] is a type of edge on a wall *)
type edge = Corner | Vert | Horiz

(**[edge_detector posA posB] returns a type edge for the edge that a projectile
   hit on a wall*)
let edge_detector posA posB=
  let vert =  floor (fst posA)|> (-.) (floor (fst posB)) |> 
              Float.abs |> Float.to_int <> 0 in
  let horiz = floor (snd posA)|> (-.) (floor (snd posB)) |> 
              Float.abs |> Float.to_int <> 0 in
  if vert && horiz then Corner else if vert then Vert else Horiz 

(**[proj_phys_engine proj walls] returns None if a proj is not in a wall and
   Some proj if it is with modified proj values.*)
let rec proj_phys_engine proj walls=
  match walls with
  | [] -> Some proj
  | h::t -> if wall_detect proj.loc h then match proj.weap_species with
      | Bouncy -> if proj.health <> 1 then 
          (match edge_detector proj.loc proj.past_loc with
           | Vert -> Some {proj with velocity = (fst proj.velocity *. (-1.0), 
                                                 snd proj.velocity); 
                                     health = proj.health-1;}
           | Horiz -> Some {proj with velocity = (fst proj.velocity, 
                                                  snd proj.velocity *. (-1.0));
                                      health = proj.health-1;}
           | Corner -> None)
        else None
      | Bullet -> None
    else proj_phys_engine proj t

(**[check_tank_wall tanks walls] returns list of tanks after performing 
   interactions with walls*)
let rec check_tank_wall (tanks:Movable.tank list) walls : Movable.tank list=
  match tanks with
  | [] -> [] 
  | h::t -> tank_phys_engine tanks h walls :: check_tank_wall t walls

(**[check_proj_wall walls projs] returns a list of projs after performing
   interactions with walls*)
let rec check_proj_wall walls (projs:Movable.projectile list)
  : Movable.projectile list=
  match projs with
  | [] -> []
  | h::t -> match proj_phys_engine h walls with
    | None -> check_proj_wall walls t
    | Some proj -> proj::check_proj_wall walls t

(**[hitbox_detect tank projs] takes in a tank and tests if any projectiles are
   in the tank's hitbox and returns true if a projectile hit a tank and false
   if it didn't*)
let hitbox_detect (tanks:Movable.tank list) (proj:Movable.projectile) : 
  Movable.tank list= 
  List.filter (fun (tank:Movable.tank) -> 
      get_distance_from proj.loc tank.loc > 0.4) tanks

(**[tank removal projs tanks] takes in a list of tanks and projectiles and 
   returns back the list of active tanks and removes tanks hit by projectiles*)
let rec tank_removal (projs:Movable.projectile list) (tanks:Movable.tank list) :
  Movable.tank list= 
  match projs with
  | [] -> tanks
  | h::t -> tank_removal t (hitbox_detect tanks h)

(**[tank_detect tanks proj] takes in a list of tanks and a projectile and checks
   to see if a projectile should be removed due to it hitting a tank via bool *)
let tank_detect (tank: Movable.tank) (projs: Movable.projectile list) :
  Movable.projectile list=
  List.filter (fun (p:Movable.projectile) -> 
      get_distance_from p.loc tank.loc > 0.4) projs

(**[proj_detect proj projs] takes in a list of projectiles and a projectile and
   checks to see if any projectiles share a spare with one another, thus 
   removing both of them*)
let rec proj_detect (proj: Movable.projectile) (projs: Movable.projectile list): 
  Movable.projectile list=
  match projs with 
  | [] -> []
  | p::t -> if get_distance_from p.loc proj.loc < 0.1 && p <> proj then 
      proj_detect proj t else p::proj_detect proj t

(**[proj_removal projs tanks obs] takes in a list of projs, tanks, and walls and
   returns back an active list of projectiles *)
let rec proj_removal (projs:Movable.projectile list) (tanks:Movable.tank list) 
  : Movable.projectile list = 
  match tanks with
  | [] -> projs
  | h::t -> proj_removal (tank_detect h projs) t 

(**[proj_removal2 projs proj] takes in a list of projs, tanks, and walls and
   returns back an active list of projectiles *)
let rec proj_removal2 (projs:Movable.projectile list) 
    (projcopy:Movable.projectile list) : Movable.projectile list = 
  match projs with
  | [] -> projcopy
  | h::t -> proj_removal2 t (proj_detect h projcopy)

(**[entity_removal_execute w st] returns a state of the game after removing
   entities*) 
let entity_removal_execute w (st:State.state)=
  {st with tanks = tank_removal st.projectiles st.tanks; 
           projectiles = let projs = (proj_removal st.projectiles st.tanks) in
             proj_removal2 projs projs |> check_proj_wall w.wall_list 
  }

(**[wall_execute w st] returns a state of the game after performing
   wall interactions on entities*) 
let wall_execute w (st:State.state)=
  {st with tanks = check_tank_wall st.tanks (w.wall_list @ w.ditch_list);}

(**[execute w st] returns a state of the game after moving entities*) 
let execute w (st:State.state)= 
  {st with tanks = move_tank st.tanks;
           projectiles = move_projs st.projectiles;}
