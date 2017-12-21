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

(** A shape is made by stroking or filling a path, or by typesetting content.
    Stroke takes one more argument to describe the style of lines. *)
type shape
val stroke : outline -> path -> shape
val fill : path -> shape
val typeset : ('a, Wall_tex.t) typesetter -> 'a -> shape

(** {0 Drawing tasks}

    To get something to the screen, a shape has to be scheduled from drawing.
    Scheduling is done by creating tasks.

    The initial task is obtained by starting a new frame.  Then new tasks can
    be scheduled by either [draw] and [group] primitives.
    These primitives take the task to start from as argument: when this task is
    done, the renderer will pick one of the task that was scheduled after.

    Thus, the tasks can describe a partial order of things to render.
    A group of tasks can be defined either as "total" or "partial":
    - in total mode, drawing happen in the same order the functions are called,
    - in partial mode, the renderer will chose an ordering that is more
      efficient for the GPU while respecting the partial order that was given.
*)

type task
type order = [ `Partial | `Total ]

(** [draw task ?frame ?quality transform paint shape] schedule a drawing task.

    After [task] is done, [shape] will drawn with style [paint] and under the
    transformation described by [transform].

    The optional [frame] argument allows to clip the rendering.
    The [quality] argument is used together with the scale factor of
    [transform] to compute a [Path.level_of_detail].
*)
val draw : task -> ?frame:frame -> ?quality:float
  -> transform -> Wall_tex.t paint -> shape
  -> task

(** [group ?order ?after task] create a new group of tasks to be scheduled
    after [task].

    If [order] is [`PARTIAL], the tasks of this group won't have a particular
    order between each other.
    If [order] is [`TOTAL], the tasks of this group will be drawn in the order
    of the calls.

    If [after] is [true], the group will be drawn after any tasks (including
    other groups) that were added to [task].
*)
val group : ?order:order -> ?after:bool -> task -> task

(** {0 Drawing context}

    A drawing context allocates the OpenGL resources that are necessary to
    render contents.  All tasks are drawn to a drawing context.  It should
    created once and reused for each frame.

    At the beginning of a frame, a new task is created.  Operations that should
    be rendered on this frame should be scheduled after this task.

    When you are done with the frame, call [flush_frame] to send all the data
    to the GPU.
    Note: this might change in the future, the renderer might just wait for a
      big enough batch to be available before sending it to the GPU in a
      streaming fashion.

    Tasks are valid for a single frame only, they are short-lived.  Reusing the
    tasks from a frame after it is rendered will raise an Invalid_argument
    exception.
*)

type t

(* [create ~antialias] initializes a renderer.
   [antialias] determines whether antialiasing is on or off, though it is
   strongly recommended to turn it on.
*)
val create : ?antialias:bool -> ?stencil_strokes:bool -> unit -> t

(* [delete t] release all the resources associated to the drawing context [t].
   It is incorrect to use this context after the call.

   A context can be retain a lot of memory, so it is good practice to release
   it if you are no longer going to use it.
*)
val delete : t -> unit

(* Schedule a new task on the drawing context.

   Tasks from a previous frame are invalidated. *)
val new_frame : ?order:order -> t -> Gg.size2 -> task

(* Flush the content of the frame to the GPU.
   All tasks that were scheduled to the last [new_frame] calls will be
   rendered.  *)
val flush_frame : t -> unit

(** {0 Convenient definitions} *)

val pi : float

val stroke_path : outline -> (Path.ctx -> unit) -> shape

val fill_path : (Path.ctx -> unit) -> shape

val draw' : task -> ?frame:frame -> ?quality:float -> transform -> Wall_tex.t paint -> shape -> unit

(** [simple_text ?frame ?halign ?valign font ~x ~y text]

    Creates a shape that represents [text] drawn using [font] at position
    [x,y].

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

    The optional [frame] argument allows to clip the rendering.
*)
val simple_text
  :  ?halign:[`LEFT | `CENTER | `RIGHT]
  -> ?valign:[`TOP | `MIDDLE | `BOTTOM | `BASELINE]
  -> font -> x:float -> y:float -> string -> shape
