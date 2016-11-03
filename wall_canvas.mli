(*
   Copyright (c) 2015 Frédéric Bour <frederic.bour@lakaban.net>

   This software is provided 'as-is', without any express or implied
   warranty.  In no event will the authors be held liable for any damages
   arising from the use of this software.
   Permission is granted to anyone to use this software for any purpose,
   including commercial applications, and to alter it and redistribute it
   freely, subject to the following restrictions:
   1. The origin of this software must not be misrepresented; you must not
      claim that you wrote the original software. If you use this software
      in a product, an acknowledgment in the product documentation would be
      appreciated but is not required.
   2. Altered source versions must be plainly marked as such, and must not be
      misrepresented as being the original software.
   3. This notice may not be removed or altered from any source distribution.
*)

open Wall

type t

val create_gl : antialias:bool -> t
val delete : t -> unit

val new_path : t -> transform -> unit

val set_winding : t -> [< `HOLE | `SOLID | `CW | `CCW ] -> unit

val move_to : t -> x:float -> y:float -> unit

val line_to : t -> x:float -> y:float -> unit

val bezier_to : t -> c1x:float -> c1y:float ->
                     c2x:float -> c2y:float ->
                     x:float   -> y:float   -> unit

val quad_to : t -> cx:float -> cy:float ->
                   x:float  -> y:float  -> unit

val rect : t -> x:float -> y:float ->
                w:float -> h:float -> unit

val round_rect : t -> x:float -> y:float ->
                      w:float -> h:float -> r:float -> unit

val circle : t -> cx:float -> cy:float -> r:float -> unit

val ellipse : t -> cx:float -> cy:float ->
                   rx:float -> ry:float -> unit

val arc : t -> cx:float -> cy:float -> r:float ->
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
