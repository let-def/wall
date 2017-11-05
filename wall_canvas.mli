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

module Path : sig
  type ctx

  val set_winding : ctx -> [< `HOLE | `SOLID | `CW | `CCW ] -> unit

  val move_to : ctx -> x:float -> y:float -> unit

  val line_to : ctx -> x:float -> y:float -> unit

  val bezier_to : ctx -> c1x:float -> c1y:float ->
    c2x:float -> c2y:float ->
      x:float   -> y:float   -> unit

  val quad_to : ctx -> cx:float -> cy:float ->
    x:float  -> y:float  -> unit

  val rect : ctx -> x:float -> y:float ->
    w:float -> h:float -> unit

  val round_rect : ctx -> x:float -> y:float ->
    w:float -> h:float -> r:float -> unit

  val circle : ctx -> cx:float -> cy:float -> r:float -> unit

  val ellipse : ctx -> cx:float -> cy:float ->
    rx:float -> ry:float -> unit

  val arc : ctx -> cx:float -> cy:float -> r:float ->
    a0:float -> a1:float -> [< `CW | `CCW ] -> unit

  val arc_to : ctx -> x1:float -> y1:float ->
    x2:float -> y2:float -> r:float -> unit

  val close : ctx -> unit
end
type path
val path : (Path.ctx -> unit) -> path

type shape
val stroke : outline -> path -> shape
val fill   : path -> shape

val stroke_path : outline -> (Path.ctx -> unit) -> shape
val fill_path   : (Path.ctx -> unit) -> shape

type t

val create_gl : antialias:bool -> t
val delete : t -> unit

val draw : t -> ?frame:frame -> ?quality:float -> transform -> Wall_tex.t paint -> shape -> unit

val text : t -> ?frame:frame -> ?halign:[`LEFT | `CENTER | `RIGHT]
                             -> ?valign:[`TOP | `MIDDLE | `BOTTOM | `BASELINE]
                             -> unit paint -> font -> x:float -> y:float -> string -> unit

val new_frame : t -> unit
val flush_frame : t -> Gg.size2 -> unit

val pi : float
