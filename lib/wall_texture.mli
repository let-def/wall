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

type t
val release : t -> unit

val flip_image : 'a Stb_image.t -> unit
val from_image : name:string -> 'a Stb_image.t -> t
val load_image :
  ?float:bool -> ?alpha:bool -> ?flip:bool -> ?name:string -> string ->
  (t, [`Msg of string]) Result.result

val channels : t -> int
val width : t -> int
val height : t -> int

val update : t -> 'a Stb_image.t -> unit

(* Internal *)
val tex : t -> Wall__backend.Texture.specification
