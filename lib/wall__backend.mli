open Wall_types
type state

val create : antialias:bool -> state
val delete : state -> unit

module Texture : sig
  type t = int

  type specification = {
    gl_tex : int;
    premultiplied : bool;
    channels : int;
  }

  val create : state -> t
  val delete : state -> t -> unit

  val upload : ?level:int -> state -> _ Stb_image.t -> t -> unit
  val update : ?level:int -> state -> x:int -> y:int -> _ Stb_image.t -> t -> unit

  val generate_mipmap : state -> t -> unit
end

module Fill : sig
  val prepare_stencil : state -> unit
  val draw_stencil : state -> first:int -> count:int -> unit

  val prepare_cover : state -> ('tex -> Texture.specification) -> 'tex paint -> frame -> unit

  val prepare_aa : state -> unit
  val draw_aa : state -> first:int -> count:int -> unit

  val finish_and_cover : state -> first:int -> count:int -> unit
end

module Convex_fill : sig
  val prepare : state -> ('tex -> Texture.specification) -> 'tex paint -> frame -> unit

  val draw : state -> first:int -> count:int -> unit
  val draw_aa : state -> first:int -> count:int -> unit
end

module Stencil_stroke : sig
  val prepare_stencil : state -> ('tex -> Texture.specification) -> 'tex paint -> frame -> float -> unit
  val draw_stencil : state -> first:int -> count:int -> unit

  val prepare_aa : state -> ('tex -> Texture.specification) -> 'tex paint -> frame -> float -> unit
  val draw_aa : state -> first:int -> count:int -> unit

  val prepare_clear : state -> unit
  val draw_clear : state -> first:int -> count:int -> unit

  val finish : state -> unit
end

module Direct_stroke : sig
  val prepare : state -> ('tex -> Texture.specification) -> 'tex paint -> frame -> float -> unit
  val draw : state -> first:int -> count:int -> unit
end

module Triangles : sig
  val prepare : state -> ('tex -> Texture.specification) -> 'tex paint -> frame -> unit
  val draw : state -> first:int -> count:int -> unit
end

type bigarray = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
val prepare : state -> width:float -> height:float -> bigarray -> unit
val set_xform : state -> transform -> unit
val finish : state -> unit

val memory_spent: unit -> int
val time_spent: unit -> int
