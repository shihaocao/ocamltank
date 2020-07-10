(** 
    Representation of in-game blocks. 
*)

(**[block_type] is a variant of wall and ditch*)
type block_type = Wall | Ditch

(**type block is a record of a block's id, kind, width, and coordinates *)
type block = {
  id : string;
  kind : block_type;
  width : float;
  (* Walls should be centrally located at multiples of n + 0.5 *)
  coord : float * float;
}