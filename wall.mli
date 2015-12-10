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
  val white : _ t
  val black : _ t
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
    xform          : Transform.t;
    scissor_xform  : Transform.t;
    scissor_extent : size2;
    alpha          : float;
  }

  val default : t

  val apply_transform : Transform.t -> t -> t
  val reset_transform : t -> t
  val translate : float -> float -> t -> t
  val rotate    : float -> t -> t
  val skew      : float -> float -> t -> t
  val scale     : float -> float -> t -> t
  val scissor   : float -> float -> float -> float -> t -> t
  val intersect_scissor : float -> float -> float -> float -> t -> t
  val reset_scissor : t -> t
end

type transform = Transform.t
type 'image paint = 'image Paint.t
type outline = Outline.t
type frame = Frame.t
