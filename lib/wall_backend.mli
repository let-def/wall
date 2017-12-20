open Wall
type t

val create : antialias:bool -> t
val delete : t -> unit

module Fill : sig
  val prepare_stencil : t -> Transform.t -> unit
  val draw_stencil : int -> int -> unit

  val prepare_cover : t -> Wall_tex.t Paint.t -> Frame.t -> float -> unit

  val prepare_aa : unit -> unit
  val draw_aa : int -> int -> unit

  val finish_and_cover : int -> int -> unit
end

module Convex_fill : sig
  val prepare : t -> Transform.t -> Wall_tex.t Paint.t -> Frame.t -> float -> unit

  val draw : int -> int -> unit
  val draw_aa : int -> int -> unit
end

module Stencil_stroke : sig
  val prepare_stencil : t -> Transform.t -> Wall_tex.t Paint.t -> Frame.t -> float -> unit
  val draw_stencil : int -> int -> unit

  val prepare_aa : t -> Wall_tex.t Paint.t -> Frame.t -> float -> unit
  val draw_aa : int -> int -> unit

  val prepare_clear : unit -> unit
  val draw_clear : int -> int -> unit

  val finish : unit -> unit
end

module Direct_stroke : sig
  val prepare : t -> Transform.t -> Wall_tex.t Paint.t -> Frame.t -> float -> unit
  val draw : int -> int -> unit
end

module Triangles : sig
  val prepare : t -> Transform.t -> Wall_tex.t Paint.t -> Frame.t -> unit val draw : int -> int -> unit
end

val prepare : t -> float -> float -> Wall_geom.B.bigarray -> unit
val set_reversed : Transform.t -> unit
val finish : unit -> unit
