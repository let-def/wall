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

module Color : sig
  include module type of struct include Color end

  val hsl   : h:float -> s:float -> l:float -> t
  val hsla  : h:float -> s:float -> l:float -> a:float -> t

  val lerp_rgba : float -> t -> t -> t
end

module Transform : sig
  type t = {
    x00 : float;
    x01 : float;
    x10 : float;
    x11 : float;
    x20 : float;
    x21 : float;
  }

  val identity : t

  val translation : x:float -> y:float -> t
  val rotation    : a:float -> t
  val scale       : sx:float -> sy:float -> t
  val skew        : sx:float -> sy:float -> t

  val compose   : t -> t -> t
  val inverse   : t -> t
  val translate : x:float -> y:float -> t -> t
  val rotate    : float -> t -> t
  val rescale   : sx:float -> sy:float -> t -> t

  val px : t -> float -> float -> float
  val py : t -> float -> float -> float

  val point     : t -> p2 -> p2
end

module Paint : sig
  type 'image t = {
    xform   : Transform.t;
    extent  : size2;
    radius  : float;
    feather : float;
    inner   : color;
    outer   : color;
    image   : 'image option;
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
  type solidity = [ `HOLE | `SOLID ]
  type line_cap = [ `BUTT | `ROUND | `SQUARE ]
  type line_join = [ `BEVEL | `MITER | `ROUND ]

  type t = {
    stroke_width   : float;
    miter_limit    : float;
    line_join      : line_join;
    line_cap       : line_cap;
  }

  val default : t
end

module Frame : sig
  type t = {
    xform  : Transform.t;
    extent : size2;
    alpha  : float;
  }

  val default : t

  val set_scissor : x:float -> y:float -> w:float -> h:float -> Transform.t -> t -> t
  val intersect_scissor : x:float -> y:float -> w:float -> h:float -> Transform.t -> t -> t
  val reset_scissor : t -> t

  val transform : t -> Transform.t -> t
  val reset_transform : t -> t
  val translate : x:float -> y:float -> t -> t
  val rotate    : float -> t -> t
  val scale     : sx:float -> sy:float -> t -> t
end

module Font : sig
  type t = {
    glyphes     : Stb_truetype.t;
    size        : float;
    spacing     : float;
    line_height : float;
  }

  val make: ?size:float -> ?spacing:float -> ?line_height:float -> Stb_truetype.t -> t
end

type transform = Transform.t
type 'image paint = 'image Paint.t
type font = Font.t
type outline = Outline.t
type frame = Frame.t

val utf8_decode : int ref -> string -> int
(** [utf8_decode r s] returns the unicode codepoint starting at offset [!r],
    advancing [r] to the beginning of next codepoint ot [String.length s] when
    the end is reached.
    If the string was not properly encoded, [-1] is returned and [r] is
    advanced to hopefully resume parsing. *)
