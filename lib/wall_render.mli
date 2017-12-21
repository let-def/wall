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
open Wall__geom
type t

val create : antialias:bool -> stencil_strokes:bool -> debug:bool -> t
val delete : t -> unit
val antialias : t -> bool

type obj =
  | Fill   of transform * Wall_tex.t paint * frame * T.bounds * V.path list
  | Stroke of transform * Wall_tex.t paint * frame * float * V.path list
  | String :  transform * unit paint * frame * float * float * 'a typesetter * 'a -> obj

val render : t -> Gg.size2 -> B.t -> obj list -> unit
