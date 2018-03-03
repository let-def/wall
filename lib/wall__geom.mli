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

(* Float buffer *)
module B : sig
  type bigarray = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = {
    mutable data: bigarray;
    mutable cursor: int;
  }

  val make : unit -> t
  val clear : t -> unit
  val reserve : t -> int -> unit
  val release : t -> int -> unit

  val data : t -> bigarray
  val alloc : t -> int -> int
  val offset : t -> int

  val sub : t -> bigarray
end

(* Tesselator *)
module T : sig
  type t

  type winding = CW | CCW

  type path = private {
    path_first : int;
    mutable path_count : int;
    mutable path_closed : bool;
    mutable path_winding: winding;
    mutable path_convex : bool;
    mutable path_nbevel : int;
  }

  type bounds = {
    minx: float;
    miny: float;
    maxx: float;
    maxy: float;
  }

  val flag_corner : int
  val flag_bevel : int
  val flag_left : int
  val flag_innerbevel : int

  val make : unit -> t
  val flush : t -> bounds * path list
  val clear : t -> unit

  val set_tol : t -> dist:float -> tess:float -> unit
  val set_tess_tol : t -> float -> unit
  val tess_tol : t -> float

  val has_path : t -> bool
  val close_path : t -> unit
  val set_winding : t -> winding -> unit
  val observed_tol : t -> bool

  val move_to : t -> float -> float -> unit
  val line_to : t -> float -> float -> unit
  val bezier_to :
    t ->
    x1:float -> y1:float ->
    x2:float -> y2:float ->
    x3:float -> y3:float ->
    unit

  val calculate_joins : t -> width:float ->
    line_join:[ `BEVEL | `MITER | `ROUND ] -> miter_limit:float -> path list -> unit

  val get_x    : t -> int -> float
  val get_y    : t -> int -> float
  val get_flags : t -> int -> int
  val get_dx   : t -> int -> float
  val get_dy   : t -> int -> float
  val get_dlen : t -> int -> float
  val get_dmx  : t -> int -> float
  val get_dmy  : t -> int -> float

  val last_x : t -> float
  val last_y : t -> float
end

(* Vertex emitter *)
module V : sig
  type path = {
    convex: bool;
    fill_first: int;
    fill_count: int;
    stroke_first: int;
    stroke_count: int;
  }

  val fill :
    T.t -> B.t ->
    edge_antialias:bool -> scale:float ->
    T.path list -> path list

  val stroke :
    T.t -> B.t ->
    width:float ->
    line_join:[ `BEVEL | `MITER | `ROUND ] ->
    line_cap:[ `BUTT | `ROUND | `SQUARE ] ->
    miter_limit:float ->
    T.path list -> path list
end
