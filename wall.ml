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

  let average_scale {x00; x10; x01; x11; _} =
    let sx = sqrt (x00 *. x00 +. x10 *. x10) in
    let sy = sqrt (x01 *. x01 +. x11 *. x11) in
    (sx +. sy) *. 0.5

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

  let dump oc t =
    Printf.fprintf oc "{%f,%f,%f,%f,%f,%f}"
    t.x00 t.x01 t.x10 t.x11 t.x20 t.x21

  let inverse t =
    let det = t.x00 *. t.x11 -. t.x10 *. t.x01 in
    if det > -1e-6 && det < 1e-6 then
      identity
    else
      let invdet = 1.0 /. det in
      {
        x00 =    t.x11 *. invdet;
        x10 = -. t.x10 *. invdet;
        x20 =    (t.x10 *. t.x21 -. t.x11 *. t.x20) *. invdet;
        x01 = -. t.x01 *. invdet;
        x11 =    t.x00 *. invdet;
        x21 =    (t.x01 *. t.x20 -. t.x00 *. t.x21) *. invdet;
      }

  let translate ~x ~y xform =
    compose (translation ~x ~y) xform

  let rotate angle xform =
    compose (rotation angle) xform

  let rescale ~sx ~sy xform =
    compose (scale ~sx ~sy) xform
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

  let dump oc t =
    Printf.fprintf oc "{ xform = %a; extent = (%f,%f); radius = %f; feather = %f }"
    Transform.dump t.xform (Size2.w t.extent) (Size2.h t.extent) t.radius t.feather

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

  let transform t xf =
    if xf == Transform.identity then t
    else { t with xform = Transform.compose t.xform xf }
end

module Frame = struct
  type t = {
    xform  : Transform.t;
    extent : size2;
    alpha  : float;
  }

  let default = {
    xform  = Transform.identity;
    extent = Size2.v (-1.0) (-1.0);
    alpha  = 1.0;
  }

  open Transform

  let transform frame xf = {frame with xform = compose frame.xform xf}
  let reset_transform frame = {frame with xform = identity}
  let translate ~x ~y frame = {frame with xform = translate ~x ~y frame.xform}
  let rotate a frame = {frame with xform = rotate a frame.xform}
  let scale ~sx ~sy frame = {frame with xform = rescale sx sy frame.xform}

  let set_scissor ~x ~y ~w ~h xf frame =
    let w = w /. 2.0 and h = h /. 2.0 in
    let mat = {identity with x20 = x +. w; x21 = y +. h} in
    {frame with xform = compose mat xf; extent = Size2.v w h }

  let intersect_scissor ~x ~y ~w ~h xf frame =
    (* If no previous scissor has been set, set the scissor as current scissor. *)
    if Size2.w frame.extent < 0.0 then
      set_scissor x y w h xf frame
    else begin
      let ex = Size2.w frame.extent and ey = Size2.h frame.extent in
      let pxform = compose frame.xform (inverse xf) in
      let tex = ex *. abs_float pxform.x00 +. ey *. abs_float pxform.x10 in
      let tey = ex *. abs_float pxform.x01 +. ey *. abs_float pxform.x11 in

      let isect_rect (ax,ay,aw,ah) (bx,by,bw,bh) =
        let minx = max ax bx and miny = max ay by in
        let maxx = min (ax+.aw) (bx+.bw) and maxy = min (ay+.ah) (by+.bh) in
        minx, miny, max 0.0 (maxx -. minx), max 0.0 (maxy -. miny)
      in
      let (x, y, w, h) = isect_rect
          (pxform.x20 -. tex, pxform.x21 -. tey, tex *. 2.0, tey *. 2.0)
          (x, y, w, h)
      in

      set_scissor x y w h xf frame
    end

  let reset_scissor frame =
    {frame with
     xform = default.xform;
     extent = default.extent
    }
end

(* utf-8 decoding dfa, from http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ *)

let utf8d =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\
   \007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
   \b\b\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\
   \n\003\003\003\003\003\003\003\003\003\003\003\003\004\003\003\
   \011\006\006\006\005\b\b\b\b\b\b\b\b\b\b\b\
   \000\001\002\003\005\b\007\001\001\001\004\006\001\001\001\001\
   \001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\000\001\001\001\001\001\000\001\000\001\001\001\001\001\001\
   \001\002\001\001\001\001\001\002\001\002\001\001\001\001\001\001\001\001\001\001\001\001\001\002\001\001\001\001\001\001\001\001\
   \001\002\001\001\001\001\001\001\001\002\001\001\001\001\001\001\001\001\001\001\001\001\001\003\001\003\001\001\001\001\001\001\
   \001\003\001\001\001\001\001\003\001\003\001\001\001\001\001\001\001\003\001\001\001\001\001\001\001\001\001\001\001\001\001\001"

let utf8_decode index str =
  let codep = ref 0 in
  let state = ref 0 in
  let len = String.length str in
  let index' = ref !index in
  while (
    !index' < len &&
    let c = Char.code (String.get str !index') in
    let t = Char.code (String.unsafe_get utf8d c) in
    codep := (if !state <> 0 then (c land 0x3f) lor (!codep lsl 6) else (0xff lsr t) land c);
    state := Char.code (String.unsafe_get utf8d (256 + !state * 16 + t) );
    incr index';
    !state > 1
  ) do ()
  done;
  index := !index';
  if !state = 0 then !codep else (-1)

module Font = struct
  type glyph_placement = [ `Align | `Exact ]

  type t = {
    glyphes: Stb_truetype.t;
    size: float;
    blur: float;
    spacing: float;
    line_height: float;
    placement   : glyph_placement;
  }

  let make ?(size=16.0) ?(blur=0.0) ?(spacing=0.0) ?(line_height=1.0) ?(placement=`Align) glyphes =
    { glyphes; blur; size; spacing; line_height; placement }

  type metrics = {
    ascent   : float;
    descent  : float;
    line_gap : float;
  }

  let font_metrics t =
    let scale = Stb_truetype.scale_for_pixel_height t.glyphes t.size in
    let {Stb_truetype. ascent; descent; line_gap} =
      Stb_truetype.vmetrics t.glyphes in
    { ascent = float ascent *. scale;
      descent = float descent *. scale;
      line_gap = float line_gap *. scale;
    }

  let text_width t text =
    let len = String.length text in
    let index = ref 0 in
    let width = ref 0 in
    let last = ref Stb_truetype.invalid_glyph in
    while !index < len  do
      match utf8_decode index text with
      | -1 -> last := Stb_truetype.invalid_glyph
      | cp ->
        let glyph = Stb_truetype.get t.glyphes cp in
        width := !width
                 + Stb_truetype.kern_advance t.glyphes !last glyph
                 + Stb_truetype.glyph_advance t.glyphes glyph;
        last := glyph
    done;
    (float !width *. Stb_truetype.scale_for_pixel_height t.glyphes t.size)
end

type transform = Transform.t
type outline = Outline.t
type 'image paint = 'image Paint.t
type font = Font.t
type frame = Frame.t
type color = Color.t
