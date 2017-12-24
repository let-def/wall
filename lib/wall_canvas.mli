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

(** {0 Path & shape}

    Drawing in wall starts by creating paths. Paths describe a shapes.
    These shapes can then be filled or stroked.  *)

module Path : sig
  type ctx

  val level_of_detail : ctx -> float

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

  val round_rect : ctx ->
    x:float -> y:float -> w:float -> h:float -> r:float -> unit

  val round_rect' : ctx ->
    x:float -> y:float -> w:float -> h:float ->
    rtl:float -> rtr:float -> rbl:float -> rbr:float -> unit

  val circle : ctx -> cx:float -> cy:float -> r:float -> unit

  val ellipse : ctx -> cx:float -> cy:float ->
    rx:float -> ry:float -> unit

  val arc : ctx -> cx:float -> cy:float -> r:float ->
    a0:float -> a1:float -> [< `CW | `CCW ] -> unit

  val arc_to : ctx -> x1:float -> y1:float ->
    x2:float -> y2:float -> r:float -> unit

  val close : ctx -> unit
end

(** A path is made from a callback that fill a context with the contents of the
    path.

    This callback can be called zero, one or many times in a given frame:
    - the callback is invoked the first time the path is used,
    - if the path is used at different scales, the callback can be invoked with
      Path.level_of_detail taking different values
    - if the content of the path is still available from a previous frame,
      the callback is not invoked
*)
type path
val path : (Path.ctx -> unit) -> path

type node
val stroke : outline -> path -> node
val fill : path -> node
val typeset : ('a, Wall_tex.t) typesetter -> 'a -> node

val paint : Wall_tex.t Paint.t -> node -> node
val transform : Transform.t -> node -> node
val scissor : Gg.box2 -> node -> node
val reset_scissor : node -> node
val intersect_scissor : Gg.box2 -> node -> node
val alpha : float -> node -> node
val none : node
val impose : node -> node -> node
val seq : node list -> node

(** {0 Drawing context}

    A drawing context allocates the OpenGL resources that are necessary to
    render contents.
*)
type t

(* [create ~antialias] creates a new drawing context.
   [antialias] determines whether antialiasing is on or off, though it is
   strongly recommended to turn it on.
*)
val create : ?antialias:bool -> ?stencil_strokes:bool -> unit -> t

(* [delete t] release all the resources associated to the drawing context [t].
   It is incorrect to use this context after the call.

   A context can retain a lot of memory, so it is good practice to release it
   if you are no longer going to use it.
*)
val delete : t -> unit

val render : t -> width:float -> height:float -> node -> unit

(** {0 Convenient definitions} *)

val pi : float

val stroke_path : outline -> (Path.ctx -> unit) -> node

val fill_path : (Path.ctx -> unit) -> node

(** [simple_text ?frame ?halign ?valign font ~x ~y text] is a shape that
    represents [text] drawn using [font] at position [x,y].

    The optionals [halign] and [valign] arguments describe how the text should
    be positioned w.r.t point [x,y].

    halign values:
    - [`LEFT], text will start at coordinate [x] ([x] is the leftmost point)
    - [`CENTER], text will be centered around [x]
    - [`RIGHT], text will end at coordinate [x] ([x] is the rightmost point)

    valign values:
    - [`TOP], top of the text will be at coordinate [y], drawing will go below
    - [`MIDDLE], text will be vertically centered at coordinate [y]
    - [`BOTTOM], bottom of the text will be at coordinate [y], drawing will be
      above
    - [`BASELINE], the baseline of the text will be at coordinate [y], most
      letters will be above but descender (as in letters such as y, p, j, q)
      will go below.
*)
val simple_text
  :  ?halign:[`LEFT | `CENTER | `RIGHT]
  -> ?valign:[`TOP | `MIDDLE | `BOTTOM | `BASELINE]
  -> font -> x:float -> y:float -> string -> node
