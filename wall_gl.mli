open Wall
open Wall_geom
type t

val create : antialias:bool -> stencil_strokes:bool -> debug:bool -> t
val delete : t -> unit

type obj =
  | Fill of Wall_tex.t paint * frame * T.bounds * V.path list
  | Stroke of Wall_tex.t paint * frame * float * V.path list

val render : t -> Gg.size2 -> B.t -> obj list -> unit
