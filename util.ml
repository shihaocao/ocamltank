(** A module of mathematical operations on tuples, floats, and ints
    to be referenced by other modules. *)

(** [diff (a,b) (x,y)] is the tuple difference of two int tuples. *)
let diff (a,b) (x,y) =
  (a-x, b-y)

(** [fdiff (a,b) (x,t)] is the tuple difference of two float tuples. *)
let fdiff (a,b) (x,y) =
  (a -. x, b -. y)

(** [fsum (a,b) (x,y)] is the tuple sum of two float tuples. *)
let fsum (a,b) (x,y) =
  (a +. x, b +. y)

(** [mult_inv flt] is the multiplicative inverse of [flt]. *)
let mult_inv flt = 
  1.0 /. flt

(** [magn (a,b)] is the magnitude of the vector [(a,b)]. *)
let magn (a,b) =
  sqrt (a *. a +. b *. b)

(** [fscale (a,b) scale_fact] is a vector in the direction of [(a,b)] 
    multiplied by [scale_fact]. *)
let fscale (a,b) scale_fact =
  (a *. scale_fact, b *. scale_fact) 

(** [fcompare x y] is true when [x] and [y] are within 0.000001 from each other.
    Otherwise, it is false. *)
let fcompare x y = 
  Float.abs (x -. y) <= 0.000001

(** [compare_pairs (a,b) (x,y)] is true when [a] and [b] and [x] and [y] are 
    within 0.000001 from each other. Otherwise, it is false. *)
let compare_pairs (a, b) (x, y) =
  fcompare a x && fcompare b y

(** [pfloor (a,b)] is the grid location [(x,y)] cooresponding to a coord 
    [(a,b)]*)
let pfloor (a,b) = 
  (int_of_float (floor a), int_of_float (floor b))

(**[comp_pair (a, b) (x, y)] is a bool of the equality status of two tuples *)
let comp_pair (a, b) (x, y) =
  a = x && b = y

(**[comp_pair (a, b) (x, y)] is an int of 0 if two tuples are equal and -1 if
   the first is less than the second and 1 otherwise*)
let comp_pair_s (a, b) (x, y) = 
  if comp_pair (a,b) (x,y) then 0 
  else if a < x then -1 
  else 1

(** [unit_vec vec] is a vector in the direction of [vec] with length 1. *)
let unit_vec vec = 
  let magn_inv = magn vec |> mult_inv in 
  fscale vec magn_inv

(**[get_distance_from pointA pointB] calculates the distance from 2 points*)
let get_distance_from pointA pointB =
  sqrt(Float.pow (fst(pointA) -. fst(pointB)) 2.0 +. 
       Float.pow (snd(pointA) -. snd(pointB)) 2.0)

(**[range_helper target iter acc] is a list of integers spanning from the first
   iter to target*)
let rec range_helper target iter acc =
  match target, iter with
  | t, i when t < 1 -> failwith "target must be > 0"
  | t, i when t = i -> acc 
  | t, i -> range_helper t (i+1) (i::acc)

(** [range target] is a list from [0; 1; 2; ... target-2; target-1]*)
let range target =
  range_helper target 0 [] |> List.rev

(** [range target] is a list from [0.0; 1.0; 2.0; ... target -. 2.0; 
    target -. 1.0]*)
let float_range target = 
  List.map (fun x -> float_of_int x) (range (int_of_float target))
