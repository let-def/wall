(*
   Copyright (c) 2017 Frédéric Bour <frederic.bour@lakaban.net>

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

type color = Gg.color

type transform = {
  x00 : float;
  x01 : float;
  x10 : float;
  x11 : float;
  x20 : float;
  x21 : float;
}

type 'texture paint = {
  xform   : transform;
  extent  : size2;
  radius  : float;
  feather : float;
  inner   : color;
  outer   : color;
  texture : 'texture option;
}

type outline = {
  stroke_width   : float;
  miter_limit    : float;
  line_join      : [ `BEVEL | `MITER | `ROUND ];
  line_cap       : [ `BUTT | `ROUND | `SQUARE ];
}

type frame = {
  xform  : transform;
  extent : size2;
  alpha  : float;
}
