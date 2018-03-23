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

open Gg
open Wall_types

(** Definition of colors, taken from Gg *)
module Color : sig
  include module type of struct include Color end

  (* Beware:
     Lots of operators (e.g. [gray]) take SRGB values, not linear RGB.
  *)

  val hsl   : h:float -> s:float -> l:float -> t
  val hsla  : h:float -> s:float -> l:float -> a:float -> t

  (* Linear interpolation between to (linear) RGBA values. *)
  val lerp_rgba : float -> t -> t -> t
end

(** Definition of affine transformation matrices. *)
module Transform : sig

  type t = transform = {
    x00 : float;
    x01 : float;
    x10 : float;
    x11 : float;
    x20 : float;
    x21 : float;
  }

  val identity : t
  val average_scale : t -> float
  val scale_x : t -> float
  val scale_y : t -> float

  val translation : x:float -> y:float -> t
  val rotation    : a:float -> t
  val scale       : sx:float -> sy:float -> t
  val skew        : sx:float -> sy:float -> t

  val compose   : t -> t -> t
  val inverse   : t -> t
  val translate : x:float -> y:float -> t -> t
  val rotate    : float -> t -> t
  val rescale   : sx:float -> sy:float -> t -> t

  (** [px t x y] is the x coordinate of the point (x,y) after applying the
      affine transformation [t]. *)
  val px : t -> float -> float -> float

  (** [py t x y] is the y coordinate of the point (x,y) after applying the
      affine transformation [t]. *)
  val py : t -> float -> float -> float

  (** [linear_px t x y] is the x coordinate of the point (x,y) after applying
      the linear transformation described by [t]. Translation is ignored! *)
  val linear_px : t -> float -> float -> float

  (** [linear_py t x y] is the y coordinate of the point (x,y) after applying
      the linear transformation described by [t]. Translation is ignored! *)
  val linear_py : t -> float -> float -> float

  (** [point t p] is the point [p] after transformation by [t] *)
  val point     : t -> p2 -> p2
end


(** {Wall drawing model}
 *
 * Drawing in wall is achieved by intersecting a simple, infinite image with a
 * shape.
 *
 * This image is described by a ['a Paint.t] value and is simple by nature:
 * - a single color,
 * - a few different kinds of gradients,
 * - some user-defined pattern or textures, as determined by ['a].
 *
 * The ['a] depends on the renderer and in practice it will be a
 * [Wall_texture.t], an abstraction over OpenGL texture.
 *
 * The shapes are made from a [Path.t] that is filled or stroked.
 * The path is list of points that are connected by straight or curved (bezier)
 * lines.
 * When filled, the path is interpreted as the contour of a surface and the
 * resulting image is this surface.
 * When stroked, the path is interpreted as one or more lines: an [Outline.t]
 * that describe the style of line rendering (thickness, square or round ends,
 * etc) is used transform the abstract lines into a surface.
 *)

module Paint : sig
  type 'texture t = 'texture paint = {
    xform   : Transform.t;
    extent  : size2;
    radius  : float;
    feather : float;
    inner   : color;
    outer   : color;
    texture : 'texture option;
  }
  val linear_gradient :
    sx:float -> sy:float ->
    ex:float -> ey:float ->
    inner:color -> outer:color -> _ t
  val radial_gradient :
    cx:float -> cy:float ->
    inr:float -> outr:float ->
    inner:color -> outer:color -> _ t
  val box_gradient :
    x:float -> y:float ->
    w:float -> h:float ->
    r:float -> f:float ->
    inner:color -> outer:color -> _ t
  val image_pattern :
    p2 -> size2 -> angle:float -> alpha:float ->
    'image -> 'image t

  val color : color -> _ t
  val rgba  : float -> float -> float -> float -> _ t
  val rgbai : int -> int -> int -> int -> _ t

  val white : _ t
  val black : _ t

  val transform : 'a t -> Transform.t -> 'a t
end

module Outline : sig
  type line_cap = [ `BUTT | `ROUND | `SQUARE ]
  type line_join = [ `BEVEL | `MITER | `ROUND ]

  type t = outline = {
    stroke_width   : float;
    miter_limit    : float;
    line_join      : line_join;
    line_cap       : line_cap;
  }

  val default : t

  val make : ?miter_limit:float -> ?join:line_join -> ?cap:line_cap -> ?width:float -> unit -> t
end

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

  type t
  val make : (ctx -> unit) -> t
end

module Texture = Wall_texture

module Typesetter : sig
  type quadbuf = {
    mutable x0: float;
    mutable y0: float;
    mutable x1: float;
    mutable y1: float;
    mutable u0: float;
    mutable v0: float;
    mutable u1: float;
    mutable v1: float;
  }

  type 'input t = {
    allocate : sx:float -> sy:float -> 'input -> (unit -> unit) option;
    render   : Transform.t -> 'input -> quadbuf -> push:(unit -> unit) -> Texture.t;
  }

  val make
    :  allocate:(sx:float -> sy:float -> 'input -> (unit -> unit) option)
    -> render:(Transform.t -> 'input -> quadbuf -> push:(unit -> unit) -> Texture.t)
    -> 'input t
end

module Image : sig
  type t

  (* Primitive images *)
  val empty : t
  val stroke : Outline.t -> Path.t -> t
  val fill : Path.t -> t
  val typeset : 'input Typesetter.t -> 'input -> t

  (* Composite images *)
  val paint : Texture.t Paint.t -> t -> t
  val transform : Transform.t -> t -> t
  val scissor : ?transform:Transform.t -> Gg.box2 -> t -> t
  val reset_scissor : t -> t
  val intersect_scissor : ?transform:Transform.t -> Gg.box2 -> t -> t
  val alpha : float -> t -> t
  val impose : t -> t -> t
  val seq : t list -> t

  (* Convenience functions *)
  val stroke_path : Outline.t -> (Path.ctx -> unit) -> t
  val fill_path : (Path.ctx -> unit) -> t
end

module Performance_counter : sig
  type t

  val make : unit -> t

  (** Nanoseconds spent rendering *)
  val time_spent : t -> int

  (** Memory words allocated *)
  val mem_spent : t -> int

  val reset : t -> unit

  val report : t -> string
end

module Renderer : sig
  (** A renderer allocates the OpenGL resources that are necessary to
      render contents.  *)
  type t

  (** [create ~antialias] creates a new drawing context.
      [antialias] determines whether antialiasing is on or off, though it is
      strongly recommended to turn it on.  *)
  val create : ?antialias:bool -> ?stencil_strokes:bool -> unit -> t

  (** Calling [delete t] releases all the resources associated to the drawing
      context [t].  It is incorrect to use this context after the call.

      A context can retain a lot of memory, so it is good practice to release
      it if you are no longer going to use it.  *)
  val delete : t -> unit

  val render :  t -> ?performance_counter:Performance_counter.t
             -> width:float -> height:float -> Image.t -> unit
end

type color     = Color.t
type transform = Transform.t
type outline   = Outline.t
type path      = Path.t
type texture   = Texture.t
type image     = Image.t
type renderer  = Renderer.t
type 'texture paint = 'texture Paint.t
type 'input typesetter = 'input Typesetter.t

val pi : float
