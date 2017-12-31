open Wall_types
type t

val create : antialias:bool -> t
val delete : t -> unit

module Texture : sig
  type t = int

  type specification = {
    gl_tex : int;
    premultiplied : bool;
    channels : int;
  }

  val create : unit -> t
  val delete : t -> unit

  val upload : ?level:int -> _ Stb_image.t -> t -> unit
  val update : ?level:int -> x:int -> y:int -> _ Stb_image.t -> t -> unit

  val generate_mipmap : t -> unit
end

module Fill : sig
  val prepare_stencil : t -> unit
  val draw_stencil : first:int -> count:int -> unit

  val prepare_cover : t -> ('tex -> Texture.specification) -> 'tex paint -> frame -> unit

  val prepare_aa : unit -> unit
  val draw_aa : first:int -> count:int -> unit

  val finish_and_cover : first:int -> count:int -> unit
end

module Convex_fill : sig
  val prepare : t -> ('tex -> Texture.specification) -> 'tex paint -> frame -> unit

  val draw : first:int -> count:int -> unit
  val draw_aa : first:int -> count:int -> unit
end

module Stencil_stroke : sig
  val prepare_stencil : t -> ('tex -> Texture.specification) -> 'tex paint -> frame -> float -> unit
  val draw_stencil : first:int -> count:int -> unit

  val prepare_aa : t -> ('tex -> Texture.specification) -> 'tex paint -> frame -> float -> unit
  val draw_aa : first:int -> count:int -> unit

  val prepare_clear : unit -> unit
  val draw_clear : first:int -> count:int -> unit

  val finish : unit -> unit
end

module Direct_stroke : sig
  val prepare : t -> ('tex -> Texture.specification) -> 'tex paint -> frame -> float -> unit
  val draw : first:int -> count:int -> unit
end

module Triangles : sig
  val prepare : t -> ('tex -> Texture.specification) -> 'tex paint -> frame -> unit
  val draw : first:int -> count:int -> unit
end

val prepare : t -> width:float -> height:float -> Wall__geom.B.bigarray -> unit
val set_xform : t -> transform -> unit
val finish : unit -> unit

val memory_spent: unit -> int
val time_spent: unit -> int
