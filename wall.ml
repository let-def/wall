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

module Utils = struct
  let clampi ~min ~max x : int =
    if x < min then min else if x > max then max else x

  let clampf ~min ~max x : float =
    if x < min then min else if x > max then max else x

  let cross ~x0 ~y0 ~x1 ~y1 = x1 *. y0 -. x0 *. y1

  let dist_pt_seg x y px py qx qy =
    let pqx = qx -. px in
    let pqy = qy -. py in
    let dx  = x -. px in
    let dy  = y -. py in
    let d = pqx *. pqx +. pqy *. pqy in
    let t = pqx *. dx +. pqy *. dy in
    let t = if d > 0. then t /. d else t in
    let t = if t < 0. then 0. else if t > 1. then 1. else t in
    let dx = px +. t *. pqx -. x in
    let dy = py +. t *. pqy -. y in
    dx *. dx +. dy*. dy

  let norm x y = sqrt (x *. x +. y *. y)
end

module Color = struct
  include Color

  let lerp_rgba u c0 c1 =
    let u = Utils.clampf ~min:0.0 ~max:1.0 u in
    let u' = 1.0 -. u in
    v (r c0 *. u' +. r c1 *. u)
      (g c0 *. u' +. g c1 *. u)
      (b c0 *. u' +. b c1 *. u)
      (a c0 *. u' +. a c1 *. u)

  let hue h ~m1 ~m2 =
    let h =
      if h < 0.0 then h +. 1.0 else
      if h > 1.0 then h -. 1.0 else
        h
    in
    if h < 1.0 /. 6.0 then
      m1 +. (m2 -. m1) *. h *. 6.0
    else if h < 3.0 /. 6.0 then
      m2
    else if h < 4.0 /. 6.0 then
      m1 +. (m2 -. m1) *. (2.0 /. 3.0 -. h) *. 6.0
    else
      m1

  let hsla ~h ~s ~l ~a =
    let h, _ = modf h in
    let h = if h < 0.0 then h +. 1.0 else h in
    let s = Utils.clampf s ~min:0.0 ~max:1.0 in
    let l = Utils.clampf l ~min:0.0 ~max:1.0 in
    let m2 = if l <= 0.5 then l *. (1.0 +. s) else l +. s -. l *. s in
    let m1 = 2.0 *. l -. m2 in
    v (Utils.clampf (hue (h +. 1.0 /. 3.0) ~m1 ~m2) ~min:0.0 ~max:1.0)
      (Utils.clampf (hue h ~m1 ~m2) ~min:0.0 ~max:1.0)
      (Utils.clampf (hue (h -. 1.0 /. 3.0) ~m1 ~m2) ~min:0.0 ~max:1.0)
      a

  let hsl ~h ~s ~l = hsla ~h ~s ~l ~a:1.0
end

module Transform = struct
  type t = {
    x00 : float;
    x01 : float;
    x10 : float;
    x11 : float;
    x20 : float;
    x21 : float;
  }

  let identity = { x00 = 1.0; x01 = 0.0;
                   x10 = 0.0; x11 = 1.0;
                   x20 = 0.0; x21 = 0.0;
                 }

  let translation ~x ~y = {identity with x20 = x; x21 = y}

  let rotation ~a =
    let c = cos a and s = sin a in {
      x00 = +.c; x01 = +.s;
      x10 = -.s; x11 = +.c;
      x20 = 0.0; x21 = 0.0;
    }

  let scale ~sx ~sy =
    {
      x00 =  sx; x01 = 0.0;
      x10 = 0.0; x11 =  sy;
      x20 = 0.0; x21 = 0.0;
    }

  let skew ~sx ~sy =
    {
      x00 = 1.0;    x01 = tan sy;
      x10 = tan sx; x11 = 1.0;
      x20 = 0.0;    x21 = 0.0;
    }

  let compose a b =
    {
      x00 = a.x00 *. b.x00 +. a.x01 *. b.x10;
      x10 = a.x10 *. b.x00 +. a.x11 *. b.x10;
      x20 = a.x20 *. b.x00 +. a.x21 *. b.x10 +. b.x20;

      x01 = a.x00 *. b.x01 +. a.x01 *. b.x11;
      x11 = a.x10 *. b.x01 +. a.x11 *. b.x11;
      x21 = a.x20 *. b.x01 +. a.x21 *. b.x11 +. b.x21;
    }

  let px t x y = (x *. t.x00 +. y *. t.x10 +. t.x20)
  let py t x y = (x *. t.x01 +. y *. t.x11 +. t.x21)

  let point t p =
    let x = P2.x p and y = P2.y p in
    P2.v (px t x y) (py t x y)

  let inverse t =
    let det = t.x00 *. t.x11 -. t.x10 *. t.x01 in
    if det > -1e-6 && det < 1e-6 then
      identity
    else
      let invdet = 1.0 /. det in
      {
        x00 =    t.x11 *. invdet;
        x10 = -. t.x10 *. invdet;
        x20 =    t.x10 *. t.x21 -. t.x11 *. t.x20 *. invdet;
        x01 = -. t.x01 *. invdet;
        x11 =    t.x00 *. invdet;
        x21 =    t.x01 *. t.x20 -. t.x00 *. t.x21 *. invdet;
      }

  let translate ~x ~y xform =
    compose xform (translation ~x ~y)

  let rotate angle xform =
    compose xform (rotation angle)

  let rescale ~sx ~sy xform =
    compose xform (scale ~sx ~sy)
end

module Outline = struct
  type solidity = [ `HOLE | `SOLID ]
  type line_join = [ `BEVEL | `MITER | `ROUND ]
  type line_cap = [ `BUTT | `ROUND | `SQUARE ]

  type t = {
    stroke_width   : float;
    miter_limit    : float;
    line_join      : line_join;
    line_cap       : line_cap;
  }

  let default = {
    stroke_width = 1.0;
    miter_limit  = 10.0;
    line_join    = `MITER;
    line_cap     = `BUTT;
  }
end

module Paint = struct
  type 'image t = {
    xform   : Transform.t;
    extent  : size2;
    radius  : float;
    feather : float;
    inner   : color;
    outer   : color;
    image   : 'image option;
  }
  let linear_gradient ~sx ~sy ~ex ~ey ~inner ~outer =
    let large = 1e5 in
    let dx = ex -. sx in
    let dy = ey -. sy in
    let d = sqrt (dx *. dx +. dy *. dy) in
    let dx, dy =
      if d > 0.0001 then
        dx /. d, dy /. d
      else
        0.0, 1.0
    in
    {
      xform = { Transform.
                x00 = dy; x01 = -.dx;
                x10 = dx; x11 = dy;
                x20 = sx -. dx *. large;
                x21 = sy -. dy *. large;
              };
      extent = Size2.v large (large +. d /. 2.0);
      radius = 0.0;
      feather = max 1.0 d;
      inner;
      outer;
      image = None;
    }

  let radial_gradient ~cx ~cy ~inr ~outr ~inner ~outer =
    let r = (inr +. outr) *. 0.5 in
    let f = outr -. inr in
    {
      xform = Transform.translation cx cy;
      extent = Size2.v r r;
      radius = r;
      feather = max 1.0 f;
      inner;
      outer;
      image = None;
    }

  let box_gradient ~x ~y ~w ~h ~r ~f ~inner ~outer =
    let w = w *. 0.5 and h = h *. 0.5 in
    {
      xform = Transform.translation (x +. w) (y +. h);
      extent = Size2.v w h;
      radius = r;
      feather = max 1.0 f;
      inner;
      outer;
      image = None;
    }

  let image_pattern p s ~angle ~alpha image =
    let cx = P2.x p and cy = P2.y p in
    let c = Color.with_a Color.white alpha in
    {
      xform = Transform.translate cx cy (Transform.rotation angle);
      extent = s;
      radius = 0.0;
      feather = 0.0;
      image = Some image;
      inner = c;
      outer = c;
    }

  let color color =
    { xform = Transform.identity; radius = 0.0; feather = 1.0;
      extent = Size2.zero; image = None;
      inner = color; outer = color }

  let rgba r g b a = color (Gg.V4.v r g b a)

  let rgbai r g b a =
    let f x = float x /. 255.0 in
    rgba (f r) (f g) (f b) (f a)

  let white = color Color.white
  let black = color Color.black
end

module Frame = struct
  type t = {
    xform          : Transform.t;
    scissor_xform  : Transform.t;
    scissor_extent : size2;
    alpha          : float;
  }

  let default = {
    xform = Transform.identity;
    scissor_xform = Transform.identity;
    scissor_extent = Size2.v (-1.0) (-1.0);
    alpha = 1.0;
  }

  open Transform

  let apply_transform xform ctx = {ctx with xform = compose xform ctx.xform}
  let reset_transform ctx = {ctx with xform = identity}
  let translate x y ctx = {ctx with xform = translate ~x ~y ctx.xform}
  let rotate a ctx = {ctx with xform = rotate a ctx.xform}
  let skew sx sy ctx = {ctx with xform = compose (skew sx sy) ctx.xform}
  let scale x y ctx = {ctx with xform = compose (scale x y) ctx.xform}

  let scissor x y w h ctx =
    let w = w /. 2.0 and h = h /. 2.0 in
    {ctx with
     scissor_xform = compose
         {identity with x20 = x +. w; x21 = y +. w}
         ctx.xform;
     scissor_extent = Size2.v w h
    }

  let intersect_scissor x y w h ctx =
    (* If no previous scissor has been set, set the scissor as current scissor. *)
    if Size2.w ctx.scissor_extent < 0.0 then
      scissor x y w h ctx
    else begin
      let pxform = compose ctx.scissor_xform (inverse ctx.xform) in
      let w = Size2.w ctx.scissor_extent in
      let h = Size2.h ctx.scissor_extent in
      let tex = w *. pxform.x00 +. h *. pxform.x10 in
      let tey = w *. pxform.x01 +. h *. pxform.x11 in

      let isect_rect (ax,ay,aw,ah) (bx,by,bw,bh) =
        let minx = max ax bx and miny = max ay by in
        let maxx = min (ax+.aw) (bx+.bw) and maxy = min (ay+.ah) (by+.bh) in
        minx, miny, max 0.0 (maxx -. minx), max 0.0 (maxy -. miny)
      in
      let (x, y, w, h) = isect_rect
          (pxform.x20 -. tex, pxform.x21 -. tey, tex *. 2.0, tey *. 2.0)
          (x, y, w, h)
      in

      scissor x y w h ctx
    end

  let reset_scissor ctx =
    {ctx with
     scissor_xform = default.scissor_xform;
     scissor_extent = default.scissor_extent
    }
end

type transform = Transform.t
type 'image paint = 'image Paint.t
type outline = Outline.t
type frame = Frame.t
