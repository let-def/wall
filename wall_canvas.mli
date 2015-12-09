open Wall

type t

val create_gl : antialias:bool -> t
val delete : t -> unit

val new_path : t -> unit

val set_winding : t -> [`CW | `CCW] -> unit

val move_to : t ->
  Wall.transform -> x:float -> y:float -> unit

val line_to : t ->
  Wall.transform -> x:float -> y:float -> unit

val bezier_to : t ->
  transform -> c1x:float -> c1y:float ->
               c2x:float -> c2y:float ->
               x:float   -> y:float   -> unit

val quad_to : t ->
  transform -> cx:float -> cy:float ->
               x:float  -> y:float  -> unit

val rect : t ->
  transform -> x:float -> y:float ->
               w:float -> h:float -> unit

val round_rect : t ->
  transform -> x:float -> y:float ->
               w:float -> h:float -> r:float -> unit

val circle : t ->
  transform -> cx:float -> cy:float -> r:float -> unit

val ellipse : t ->
  transform -> cx:float -> cy:float ->
               rx:float -> ry:float -> unit

val arc : t ->
  transform -> cx:float -> cy:float -> r:float ->
               a0:float -> a1:float -> [ `CW | `CCW ] -> unit

(*val arc_to : t ->
  transform -> x1:float -> y1:float ->
               x2:float -> y2:float -> radius:float -> unit*)

val close_path : t -> unit

val stroke : t -> ?frame:frame -> Wall_tex.t paint -> outline -> unit
val fill   : t -> ?frame:frame -> Wall_tex.t paint -> unit

val new_frame : t -> unit
val flush_frame : t -> Gg.size2 -> unit


(* Length proportional to radius of a cubic bezier handle for 90deg arcs. *)
val kappa90 : float
val pi : float
