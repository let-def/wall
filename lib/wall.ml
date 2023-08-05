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

[@@@landmark "auto"]
open Gg
open Wall_types

module Backend = Wall__backend

type renderer = {
  t : Wall__geom.T.t;
  mutable b : Wall__geom.B.t;
  mutable b_mark: unit ref;
  mutable b' : Wall__geom.B.t;
  mutable b'_mark: unit ref;
  g : Backend.state;
  antialias : bool;
  stencil_strokes : bool;
}

let pi = 3.14159265358979323846264338327
let kappa90 = 0.5522847493

module Utils = struct
  let maxf x y : float = if x >= y then x else y
  let minf x y : float = if x <= y then x else y
  let maxi x y : int   = if x >= y then x else y
  let mini x y : int   = if x <= y then x else y

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
  type t = transform = {
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

  let scale_x t = Utils.norm t.x00 t.x10
  let scale_y t = Utils.norm t.x01 t.x11

  let average_scale t = (scale_x t +. scale_y t) *. 0.5

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
    if a == identity then b
    else if b == identity then a
    else
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

  let linear_px t x y = (x *. t.x00 +. y *. t.x10)
  let linear_py t x y = (x *. t.x01 +. y *. t.x11)

  let point t p =
    let x = P2.x p and y = P2.y p in
    P2.v (px t x y) (py t x y)

  let dump oc t =
    Printf.fprintf oc "{%f,%f,%f,%f,%f,%f}"
    t.x00 t.x01 t.x10 t.x11 t.x20 t.x21

  let inverse t =
    if t == identity then identity
    else
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
  type line_join = [ `BEVEL | `MITER | `ROUND ]
  type line_cap = [ `BUTT | `ROUND | `SQUARE ]

  type t = outline = {
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

  let make ?(miter_limit=10.0) ?(join=`MITER) ?(cap=`BUTT) ?(width=1.0) () =
    { miter_limit; line_join = join; line_cap = cap; stroke_width = width }
end

module Paint = struct
  type 'texture t = 'texture paint = {
    xform   : Transform.t;
    extent  : size2;
    radius  : float;
    feather : float;
    inner   : color;
    outer   : color;
    texture   : 'texture option;
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
      texture = None;
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
      texture = None;
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
      texture = None;
    }

  let image_pattern p s ~angle ~alpha image =
    let cx = P2.x p and cy = P2.y p in
    let c = Color.with_a Color.white alpha in
    {
      xform = Transform.translate cx cy (Transform.rotation angle);
      extent = s;
      radius = 0.0;
      feather = 0.0;
      texture = Some image;
      inner = c;
      outer = c;
    }

  let color color =
    { xform = Transform.identity; radius = 0.0; feather = 1.0;
      extent = Size2.zero; texture = None;
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
  type t = frame = {
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

module Texture = struct
  let invalid =
    {Backend.Texture. gl_tex = -1; channels = 0; premultiplied = false}

  type t = {
    name: string;
    backend: Backend.state;
    mutable tex: Backend.Texture.specification;
    mutable width: int;
    mutable height: int;
  }

  let release t =
    if t.tex.gl_tex <> -1 then begin
      Backend.Texture.delete t.backend t.tex.gl_tex;
      t.tex <- invalid;
    end

  let finalize t =
    if t.tex.gl_tex <> -1 then begin
      prerr_endline
        ("Wall_tex warning: texture " ^ t.name ^ " has not been released");
      release t
    end

  let validate t =
    if t.tex.gl_tex = -1 then
      invalid_arg ("Wall_tex: " ^ t.name ^ " has been released")

  let tex t = validate t; t.tex

  let flip_image
      (type a) (type b)
      ({Stb_image. channels; width; height; data} :
         (a, b) Bigarray.kind Stb_image.t) =
    match Bigarray.Array1.kind data with
    | Bigarray.Float32 ->
      let stride = width * channels in
      let half_height = height / 2 in
      for row = 0 to half_height - 1 do
        let top = stride * row in
        let bot = stride * (height - row - 1) in
        for col = 0 to stride - 1 do
          let a = data.{top + col} in
          let b = data.{bot + col} in
          data.{top + col} <- b;
          data.{bot + col} <- a;
        done
      done
    | Bigarray.Int8_unsigned ->
      let stride = width * channels in
      let half_height = height / 2 in
      for row = 0 to half_height - 1 do
        let top = stride * row in
        let bot = stride * (height - row - 1) in
        for col = 0 to stride - 1 do
          let a = data.{top + col} in
          let b = data.{bot + col} in
          data.{top + col} <- b;
          data.{bot + col} <- a;
        done
      done
    | _ -> invalid_arg "Wall_tex: unsupported image format"

  let update t image =
    validate t;
    Backend.Texture.upload ~level:0 t.backend image t.tex.gl_tex;
    Backend.Texture.generate_mipmap t.backend t.tex.gl_tex;
    t.tex <- {t.tex with Backend.Texture.channels = image.Stb_image.channels};
    t.width <- image.Stb_image.width;
    t.height <- image.Stb_image.height

  let sub_update st t ~x ~y image =
    validate t;
    Backend.Texture.update ~level:0 ~x ~y st.g image t.tex.gl_tex;
    Backend.Texture.generate_mipmap st.g t.tex.gl_tex

  let from_image st ~name image =
    let tex = {
      Backend.Texture.
      gl_tex = Backend.Texture.create st.g;
      premultiplied = true;
      channels = 0
    } in
    let t = { name; backend=st.g; width = 0; height = 0; tex } in
    Gc.finalise finalize t;
    update t image;
    t

  let load_image st ?(float=false) ?(alpha=true) ?(flip=false) ?name s =
    let channels = if alpha then 4 else 3 in
    let name = match name with
      | None -> s
      | Some name -> name
    in
    let load = function
      | Result.Error _ as error -> error
      | Result.Ok image ->
        if flip then flip_image image;
        let t = from_image ~name st image in
        Stb_image.free_unmanaged image;
        Result.Ok t
    in
    if float then
      load (Stb_image.loadf_unmanaged ~channels s)
    else
      load (Stb_image.load_unmanaged ~channels s)

  let channels t = t.tex.Backend.Texture.channels
  let width t = t.width
  let height t = t.height
end


module Typesetter = struct
  type quadbuf = {
    mutable x0: float;
    mutable y0: float;
    mutable x1: float;
    mutable y1: float;
    mutable u0: float;
    mutable v0: float;
    mutable u1: float;
    mutable v1: float;
  }

  type 'input t = {
    allocate : renderer -> sx:float -> sy:float -> 'input -> (unit -> unit) option;
    render   : renderer -> Transform.t -> 'input -> quadbuf -> push:(unit -> unit) -> Texture.t;
  }

  let make ~allocate ~render =
    { allocate; render }
end

module Path = struct
  open Wall__geom


  type ctx = T.t

  let level_of_detail t =
    T.tess_tol t *. 4.0

  let set_winding t w =
    T.set_winding t (match w with `CW | `HOLE -> T.CW | `SOLID | `CCW -> T.CCW)

  let close t =
    T.close_path t

  let move_to t ~x ~y =
    T.move_to t x y

  let line_to t ~x ~y =
    T.line_to t x y

  let bezier_to t ~c1x ~c1y ~c2x ~c2y ~x ~y =
    T.bezier_to t
    ~x1:(c1x)
    ~y1:(c1y)
    ~x2:(c2x)
    ~y2:(c2y)
    ~x3:(x)
    ~y3:(y)

  let quad_to t ~cx ~cy ~x ~y =
    let x0 = T.last_x t in
    let y0 = T.last_y t in
    T.bezier_to t
    ~x1:(x0 +. 2.0 /. 3.0 *. (cx -. x0))
    ~y1:(y0 +. 2.0 /. 3.0 *. (cy -. y0))
    ~x2:(x +. 2.0 /. 3.0 *. (cx -. x))
    ~y2:(y +. 2.0 /. 3.0 *. (cy -. y))
    ~x3:x ~y3:y

  let rect t ~x ~y ~w ~h =
    move_to t ~x ~y;
    line_to t ~x ~y:(y +. h);
    line_to t ~x:(x +. w) ~y:(y +. h);
    line_to t ~x:(x +. w) ~y;
    close t

  let round_rect' t ~x ~y ~w ~h ~rtl ~rtr ~rbl ~rbr =
    if rtl +. rtr +. rbl +. rbr < 0.4 *. level_of_detail t then
      rect t ~x ~y ~w ~h
    else
      let hw = abs_float w *. 0.5 and hh = abs_float h *. 0.5 in
      begin
        let rx = copysign (Utils.minf rbl hw) w in
        let ry = copysign (Utils.minf rbl hh) h in
        move_to t ~x ~y:(y +. h -. ry);
        bezier_to t
          ~c1x:x ~c1y:(y +. h -. ry *. (1.0 -. kappa90))
          ~c2x:(x +. rx *. (1.0 -. kappa90)) ~c2y:(y +. h)
          ~x:(x +. rx) ~y:(y +. h);
      end;
      begin
        let rx = copysign (Utils.minf rbr hw) w in
        let ry = copysign (Utils.minf rbr hh) h in
        line_to t ~x:(x +. w -. rx) ~y:(y +. h);
        bezier_to t
          ~c1x:(x +. w -. rx *. (1.0 -. kappa90)) ~c1y:(y +. h)
          ~c2x:(x +. w) ~c2y:(y +. h -. ry *. (1.0 -. kappa90))
          ~x:(x +. w) ~y:(y +. h -. ry);
      end;
      begin
        let rx = copysign (Utils.minf rtr hw) w in
        let ry = copysign (Utils.minf rtr hh) h in
        line_to t ~x:(x +. w) ~y:(y +. ry);
        bezier_to t
          ~c1x:(x +. w) ~c1y:(y +. ry *. (1.0 -. kappa90))
          ~c2x:(x +. w -. rx *. (1.0 -. kappa90)) ~c2y:y
          ~x:(x +. w -. rx) ~y;
      end;
      begin
        let rx = copysign (Utils.minf rtl hw) w in
        let ry = copysign (Utils.minf rtl hh) h in
        line_to t ~x:(x +. rx) ~y;
        bezier_to t
          ~c1x:(x +. rx *. (1.0 -. kappa90)) ~c1y:y
          ~c2x:x ~c2y:(y +. ry *. (1.0 -. kappa90))
          ~x ~y:(y +. ry);
      end;
      close t

  let round_rect t ~x ~y ~w ~h ~r =
    if r < 0.1 *. level_of_detail t then
      rect t ~x ~y ~w ~h
    else begin
      let rx = copysign (Utils.minf r (abs_float w *. 0.5)) w in
      let ry = copysign (Utils.minf r (abs_float h *. 0.5)) h in
      move_to t ~x ~y:(y +. ry);
      line_to t ~x ~y:(y +. h -. ry);
      bezier_to t
        ~c1x:x ~c1y:(y +. h -. ry *. (1.0 -. kappa90))
        ~c2x:(x +. rx *. (1.0 -. kappa90)) ~c2y:(y +. h)
        ~x:(x +. rx) ~y:(y +. h);
      line_to t ~x:(x +. w -. rx) ~y:(y +. h);
      bezier_to t
        ~c1x:(x +. w -. rx *. (1.0 -. kappa90)) ~c1y:(y +. h)
        ~c2x:(x +. w) ~c2y:(y +. h -. ry *. (1.0 -. kappa90))
        ~x:(x +. w) ~y:(y +. h -. ry);
      line_to t ~x:(x +. w) ~y:(y +. ry);
      bezier_to t
        ~c1x:(x +. w) ~c1y:(y +. ry *. (1.0 -. kappa90))
        ~c2x:(x +. w -. rx *. (1.0 -. kappa90)) ~c2y:y
        ~x:(x +. w -. rx) ~y;
      line_to t ~x:(x +. rx) ~y;
      bezier_to t
        ~c1x:(x +. rx *. (1.0 -. kappa90)) ~c1y:y
        ~c2x:x ~c2y:(y +. ry *. (1.0 -. kappa90))
        ~x ~y:(y +. ry);
      close t
    end

  let ellipse t ~cx ~cy ~rx ~ry =
    move_to t ~x:(cx -. rx) ~y:cy;
    bezier_to t
      ~c1x:(cx -. rx) ~c1y:(cy +. ry *. kappa90)
      ~c2x:(cx -. rx *. kappa90) ~c2y:(cy +. ry) ~x:cx ~y:(cy +. ry);
    bezier_to t
      ~c1x:(cx +. rx *. kappa90) ~c1y:(cy +. ry)
      ~c2x:(cx +. rx) ~c2y:(cy +. ry *. kappa90) ~x:(cx +. rx) ~y:cy;
    bezier_to t
      ~c1x:(cx  +. rx) ~c1y:(cy -. ry *. kappa90)
      ~c2x:(cx +. rx *. kappa90) ~c2y:(cy -. ry) ~x:cx ~y:(cy -. ry);
    bezier_to t
      ~c1x:(cx  -. rx *. kappa90) ~c1y:(cy -. ry)
      ~c2x:(cx -. rx) ~c2y:(cy -. ry *. kappa90) ~x:(cx -. rx) ~y:cy;
    close t

  let circle t ~cx ~cy ~r =
    ellipse t ~cx ~cy ~rx:r ~ry:r

  let arc t ~cx ~cy ~r ~a0 ~a1 dir =
    let da = (a1 -. a0) in
    let da =
      if abs_float da >= 2.0 *. pi then
        match dir with
        | `CW  -> 2.0 *. pi
        | `CCW -> -. 2.0 *. pi
      else
        match dir with
        | `CW  -> if da < 0.0 then da +. 2.0 *. pi else da
        | `CCW -> if da > 0.0 then da -. 2.0 *. pi else da
    in
    let ndivs =
      Utils.clampi ~min:1 ~max:5
        (int_of_float (abs_float da /. (pi *. 0.5) +. 0.5))
    in
    (* Split arc into max 90 degree segments. *)
    let kappa =
      let hda = (da /. float ndivs) /. 2.0 in
      abs_float (4.0 /. 3.0 *. (1.0 -. cos hda) /. sin hda)
    in
    let kappa = match dir with
      | `CW  -> kappa
      | `CCW -> -.kappa
    in
    let coords i =
      let a = a0 +. da *. (float i /. float ndivs) in
      let dx = cos a and dy = sin a in
      let x = cx +. dx *. r and y = cy +. dy *. r in
      let tanx = -. dy *. r *. kappa and tany = dx *. r *. kappa in
      x, y, tanx, tany
    in
    let rec step (px, py, ptanx, ptany) i =
      if i > ndivs then () else
        let (x, y, tanx, tany) as coords = coords i in
        bezier_to t
          ~c1x:(px +. ptanx) ~c1y:(py +. ptany)
          ~c2x:(x -. tanx) ~c2y:(y -. tany)
          ~x ~y;
        step coords (i + 1)
    in
    let (x, y, _, _) as coords = coords 0 in
    if T.has_path t then
      line_to t ~x ~y
    else
      move_to t ~x ~y;
    step coords 1

  let dist_pt_seg x y px py qx qy =
    let pqx = qx -. px in
    let pqy = qy -. py in
    let dx = x -. px in
    let dy = y -. py in
    let d = pqx *. pqx +. pqy *. pqy in
    let t = pqx *. dx +. pqy *. dy in
    let t =
      if t < 0.0 then 0.0 else
        let t = if d > 0.0 then t /. d else t in
        if t > 1.0 then 1.0 else t
    in
    let dx = px +. t *. pqx -. x in
    let dy = py +. t *. pqy -. y in
    (dx *. dx +. dy *. dy)

  let arc_to t ~x1 ~y1 ~x2 ~y2 ~r =
    if T.has_path t then (
      let tol = T.tess_tol t in
      let x0 = T.last_x t and y0 = T.last_y t in
      (* Handle degenerate cases. *)
      if r < tol ||
         (abs_float (x1 -. x0) < tol && abs_float (y1 -. y0) < tol) ||
         (abs_float (x2 -. x1) < tol && abs_float (y2 -. y1) < tol) ||
         (dist_pt_seg x1 y1 x0 y0 x2 y2 < tol *. tol)
      then line_to t x1 y1
      else
        let dx0 = x0 -. x1 and dy0 = y0 -. y1 in
        let dx1 = x2 -. x1 and dy1 = y2 -. y1 in
        let n0 = 1. /. sqrt (dx0 *. dx0 +. dy0 *. dy0) in
        let n1 = 1. /. sqrt (dx1 *. dx1 +. dy1 *. dy1) in
        let dx0 = dx0 *. n0 and dy0 = dy0 *. n0 in
        let dx1 = dx1 *. n1 and dy1 = dy1 *. n1 in
        let a = acos (dx0 *. dx1 +. dy0 *. dy1) in
        let d = r /. tan (a /. 2.0) in
        (* printf("a=%f° d=%f\n", a/NVG_PI*180.0f, d); *)
        if d > 10000.0
        then line_to t x1 y1
        else (
          let cross = dx1 *. dy0 -. dx0 *. dy1 in
          if cross > 0.0
          then (
            arc t `CW ~r
              ~cx:(x1 +. dx0 *. d +. (dy0 *. r))
              ~cy:(y1 +. dy0 *. d -. (dx0 *. r))
              ~a0:(atan2 dx0 (-.dy0)) ~a1:(atan2 (-.dx1) dy1)
              (* printf("CW c=(%f, %f) a0=%f° a1=%f°\n", cx, cy, a0/NVG_PI*180.0f, a1/NVG_PI*180.0f); *)
          ) else (
            arc t `CCW ~r
              ~cx:(x1 +. dx0 *. d -. dy0 *. r)
              ~cy:(y1 +. dy0 *. d +. dx0 *. r)
              ~a0:(atan2 (-.dx0) dy0) ~a1:(atan2 dx1 (-.dy1))
              (* printf("CCW c=(%f, %f) a0=%f° a1=%f°\n", cx, cy, a0/NVG_PI*180.0f, a1/NVG_PI*180.0f); *)
          )
        )
    )

  type t = { closure : (ctx -> unit) }
  let make closure = { closure }
end

type buffer_item = {
  paths : Wall__geom.V.path list;
  triangle_offset : int;
  triangle_count  : int;
  mark: unit ref;
}

let invalid_buffer_item = {
  paths = [];
  triangle_offset=0;
  triangle_count=0;
  mark=ref ();
}

module Image = struct
  type t =
    (* Base cases *)
    | Empty
    | Fill    of {path: Path.t; mutable item: buffer_item; mutable item_lod: int}
    | Stroke  of {path: Path.t; outline: Outline.t; mutable item: buffer_item; mutable item_lod: int}
    | String  :  'a * 'a Typesetter.t -> t
    (* Recursive cases *)
    | Xform   of t * Transform.t
    | Paint   of t * Texture.t Paint.t
    | Scissor of t * Transform.t * Gg.box2 * [`Set | `Reset | `Intersect]
    | Alpha   of t * float
    | Seq     of t * t

  let empty = Empty

  let stroke outline path =
    Stroke {path; outline; item=invalid_buffer_item; item_lod = -1}

  let fill path =
    Fill {path; item=invalid_buffer_item; item_lod = -1}

  let typeset typesetter contents =
    String (contents, typesetter)

  let paint paint = function
    | Empty -> Empty
    | Paint _ as node -> node
    | node -> Paint (node, paint)

  let transform xf = function
    | node when xf == Transform.identity -> node
    | Empty -> Empty
    | Xform (node, xf') -> Xform (node, Transform.compose xf' xf)
    | node -> Xform (node, xf)

  let stack n1 n2 =
    match n1, n2 with
    | Empty, n | n, Empty -> n
    | _ -> Seq (n1, n2)

  let rec seq = function
    | [] -> Empty
    | [x] -> x
    | x :: xs -> stack x (seq xs)

  let scissor ?(transform=Transform.identity) box = function
    | Empty -> Empty
    | Scissor (_, _, _, (`Set | `Reset)) as node -> node
    | node -> Scissor (node, transform, box, `Set)

  let reset_scissor = function
    | Empty -> Empty
    | Scissor (_, _, _, (`Set | `Reset)) as node -> node
    | node -> Scissor (node, Transform.identity, Gg.Box2.empty, `Reset)

  let intersect_scissor ?(transform=Transform.identity) box = function
    | Empty -> Empty
    | node -> Scissor (node, transform, box, `Intersect)

  let alpha a = function
    | Empty -> Empty
    | Alpha (node, a') -> Alpha (node, a *. a')
    | node -> Alpha (node, a)

  let fill_path f = fill (Path.make f)
  let stroke_path o f = stroke o (Path.make f)
end

module Performance_counter : sig
  type t

  val make : unit -> t

  (** Microseconds spent rendering *)
  val time_spent : t -> int

  (** Memory words allocated *)
  val mem_spent : t -> int

  val reset : t -> unit

  val report : t -> string

  val bump : t -> prep_time:int -> prep_mem:int
               -> flush_time:int -> flush_mem:int -> unit
end = struct
  type t = {
    mutable frames: int;
    mutable prep_time: int;
    mutable prep_mem: int;
    mutable flush_time: int;
    mutable flush_mem: int;
  }

  let make () =
    { frames = 0; prep_time = 0; prep_mem = 0; flush_time = 0; flush_mem = 0 }

  (** Microseconds spent rendering *)
  let time_spent t = t.prep_time + t.flush_time

  (** Memory words allocated *)
  let mem_spent t = t.prep_mem + t.flush_mem

  let reset t =
    t.frames <- 0;
    t.prep_time <- 0;
    t.prep_mem <- 0;
    t.flush_time <- 0;
    t.flush_mem <- 0

  let report t =
    let time = time_spent t in
    let mem = mem_spent t in
    let d = max 1 t.frames in
    Printf.sprintf
      "rendered %d (total=prep+gl) frames in %d=%d+%d us (%d=%d+%d us/frame) \
       using %d=%d+%d words (%d=%d+%d w/frame)\n"
      t.frames
      time t.prep_time t.flush_time
      (time / d) (t.prep_time / d) (t.flush_time / d)
      mem t.prep_mem t.flush_mem
      (mem / d) (t.prep_mem / d) (t.flush_mem / d)

  let bump t ~prep_time ~prep_mem ~flush_time ~flush_mem =
    t.frames <- t.frames + 1;
    t.prep_time <- t.prep_time + prep_time;
    t.prep_mem <- t.prep_mem + prep_mem;
    t.flush_time <- t.flush_time + flush_time;
    t.flush_mem <- t.flush_mem + flush_mem
end

module Renderer = struct
  open Wall__geom
  open Image

  type t = renderer

  let create ?(antialias=true) ?(stencil_strokes=true) () = {
    t = T.make ();
    b = B.make ();
    b_mark = ref ();
    b' = B.make ();
    b'_mark = ref ();
    g = Wall__backend.create ~antialias;
    antialias;
    stencil_strokes;
  }

  let delete t =
    T.clear t.t;
    B.clear t.b;
    Backend.delete t.g

  let rec typesetter_prepare t acc xx xy yx yy = function
    | Empty | Fill _ | Stroke _ -> acc
    | Paint (n, _) | Alpha (n, _) | Scissor (n, _, _, _) ->
      typesetter_prepare t acc xx xy yx yy n
    | Seq (n1, n2) ->
      let acc = typesetter_prepare t acc xx xy yx yy n1 in
      typesetter_prepare t acc xx xy yx yy n2
    | Xform (n, xf) ->
      (*Printf.printf "(%f,%f) (%f,%f) -> " xx xy yx yy;*)
      let xx = Transform.linear_px xf xx xy
      and xy = Transform.linear_py xf xx xy
      and yx = Transform.linear_px xf yx yy
      and yy = Transform.linear_py xf yx yy
      in
      (*Printf.printf "(%f,%f) (%f,%f)\n%!" xx xy yx yy;*)
      typesetter_prepare t acc xx xy yx yy n
    | String (x, cls) ->
      let sx = sqrt (xx *. xx +. xy *. xy) in
      let sy = sqrt (yx *. yx +. yy *. yy) in
      match cls.allocate t ~sx ~sy x with
      | None -> acc
      | Some f -> (f :: acc)

  type prepared_node =
    (* Base cases *)
    | PFill   of buffer_item
    | PStroke of buffer_item * float
    | PString of buffer_item * Texture.t
    | PEmpty
    (* Recursive cases *)
    | PXform  of prepared_node * Transform.t
    | PPaint  of prepared_node * Texture.t paint
    | PScissor of prepared_node * Transform.t * Gg.box2 * [`Set | `Reset | `Intersect]
    | PAlpha  of prepared_node * float
    | PSeq    of prepared_node * prepared_node

  let is_convex = function
    | [path] -> path.V.convex
    | _ -> false

  let quadbuf =
    {Typesetter.
      x0 = 0.0; y0 = 0.0; x1 = 0.0; y1 = 0.0;
      u0 = 0.0; v0 = 0.0; u1 = 0.0; v1 = 0.0}

  let ( .%{}<-) (arr: B.bigarray) idx v = Bigarray.Array1.unsafe_set arr idx v;;

  let push_quad b =
    let d = B.data b and c = B.alloc b (6 * 4) in
    let q = quadbuf in
    d.%{c+ 0+0}<-q.x0; d.%{c+ 0+1}<-q.y0; d.%{c+ 0+2}<-q.u0; d.%{c+ 0+3}<-q.v0;
    d.%{c+ 4+0}<-q.x1; d.%{c+ 4+1}<-q.y1; d.%{c+ 4+2}<-q.u1; d.%{c+ 4+3}<-q.v1;
    d.%{c+ 8+0}<-q.x1; d.%{c+ 8+1}<-q.y0; d.%{c+ 8+2}<-q.u1; d.%{c+ 8+3}<-q.v0;
    d.%{c+12+0}<-q.x0; d.%{c+12+1}<-q.y0; d.%{c+12+2}<-q.u0; d.%{c+12+3}<-q.v0;
    d.%{c+16+0}<-q.x0; d.%{c+16+1}<-q.y1; d.%{c+16+2}<-q.u0; d.%{c+16+3}<-q.v1;
    d.%{c+20+0}<-q.x1; d.%{c+20+1}<-q.y1; d.%{c+20+2}<-q.u1; d.%{c+20+3}<-q.v1

  let push_quad_strip b =
    let d = B.data b and c = B.alloc b (4 * 4) in
    let q = quadbuf in
    d.%{c+ 0+0}<-q.x1; d.%{c+ 0+1}<-q.y1; d.%{c+ 0+2}<-q.u1; d.%{c+ 0+3}<-q.v1;
    d.%{c+ 4+0}<-q.x1; d.%{c+ 4+1}<-q.y0; d.%{c+ 4+2}<-q.u1; d.%{c+ 4+3}<-q.v0;
    d.%{c+ 8+0}<-q.x0; d.%{c+ 8+1}<-q.y1; d.%{c+ 8+2}<-q.u0; d.%{c+ 8+3}<-q.v1;
    d.%{c+12+0}<-q.x0; d.%{c+12+1}<-q.y0; d.%{c+12+2}<-q.u0; d.%{c+12+3}<-q.v0

  let scale_factor xf =
    let sx = Utils.norm xf.x00 xf.x10 in
    let sy = Utils.norm xf.x01 xf.x11 in
    sx *. sy

  let prepare_path t ~factor path =
    T.clear t;
    T.set_tess_tol t (0.25 /. factor);
    path.Path.closure t

  let promote_item t {paths; mark; triangle_offset; triangle_count} =
    assert (mark == t.b'_mark);
    let paths = List.map (V.copy ~from:t.b' ~to_:t.b) paths in
    let triangle_offset =
      if triangle_count > 0 then (
        let result = B.offset t.b / 4 in
        B.copy ~from:t.b' ~to_:t.b
          ~offset:(triangle_offset * 4)
          ~count:(triangle_count * 4);
        result
      ) else
        0
    in
    {paths; triangle_offset; triangle_count; mark=t.b_mark}

  let should_promote_item t item =
    item.mark == t.b'_mark

  let is_item_valid t item =
    item.mark == t.b_mark || item.mark == t.b'_mark

  let rec prepare t xf = function
    (* Base cases *)
    | Empty -> PEmpty
    | Fill x ->
      let factor = scale_factor xf in
      let _, lod = frexp factor in
      if is_item_valid t x.item && x.item_lod >= lod then (
        if should_promote_item t x.item then
          x.item <- promote_item t x.item;
        PFill x.item
      ) else (
        prepare_path t.t ~factor x.path;
        let bounds, paths = T.flush t.t in
        let paths =
          V.fill t.t t.b
            ~edge_antialias:t.antialias
            ~scale:(1.0 /. Transform.average_scale xf)
            paths
        in
        let item =
          if is_convex paths then (
            {paths; triangle_offset = 0; triangle_count = 0; mark = t.b_mark}
          ) else (
            let {T. minx; miny; maxx; maxy} = bounds in
            B.reserve t.b (4 * 4);
            let triangle_offset = B.offset t.b / 4 in
            let q = quadbuf in
            q.x0 <- minx; q.y0 <- miny;
            q.x1 <- maxx; q.y1 <- maxy;
            q.u0 <-  0.5; q.v0 <-  1.0;
            q.u1 <-  0.5; q.v1 <-  1.0;
            push_quad_strip t.b;
            {paths; triangle_offset; triangle_count = 4; mark = t.b_mark}
          )
        in
        x.item <- item;
        x.item_lod <- lod;
        PFill item
      )
    | Stroke x ->
      let {Outline. stroke_width; miter_limit; line_join; line_cap} = x.outline in
      let factor = scale_factor xf in
      let _, lod = frexp factor in
      if is_item_valid t x.item && x.item_lod >= lod then (
        if should_promote_item t x.item then
          x.item <- promote_item t x.item;
        PStroke (x.item, stroke_width)
      ) else (
        prepare_path t.t ~factor x.path;
        let _bounds, paths = T.flush t.t in
        let paths =
          V.stroke t.t t.b
            ~width:stroke_width
            ~miter_limit
            ~line_join
            ~line_cap
            paths
        in
        let item = {paths; triangle_offset = 0; triangle_count = 6; mark=t.b_mark} in
        x.item <- item;
        x.item_lod <- lod;
        PStroke (item, stroke_width)
      )
    | String (x, cls) ->
      let vbuffer = t.b in
      let offset = B.offset vbuffer in
      begin match cls.Typesetter.render t xf x quadbuf
                    (fun () ->
                       B.reserve vbuffer (6 * 4);
                       push_quad vbuffer)
        with
        | exception _ -> PEmpty
        | texture ->
          let triangle_offset = offset / 4 in
          let triangle_count  = (B.offset vbuffer - offset) / 4 in
          PString ({paths = []; triangle_offset; triangle_count; mark=t.b_mark}, texture)
      end
    (* Recursive cases *)
    | Xform  (n, xf') ->
      let xf = Transform.compose xf' xf in
      PXform (prepare t xf n, xf)
    | Paint  (n, p) ->
      PPaint (prepare t xf n, Paint.transform p xf)
    | Scissor (n, xf', box, action) ->
      PScissor (prepare t xf n, xf', box, action)
    | Alpha (n, a) ->
      PAlpha (prepare t xf n, a)
    | Seq (n1, n2) ->
      PSeq (prepare t xf n1, prepare t xf n2)

  let xform_outofdate = ref true

  let rec exec t xf paint frame = function
    | PFill _ | PStroke _ | PString _ when
        !xform_outofdate &&
        (Backend.set_xform t.g xf; xform_outofdate := false; false) -> assert false
    | PFill { paths = [path]; triangle_offset = 0; triangle_count = 0 } ->
      Backend.Convex_fill.prepare t.g Texture.tex paint frame;
      Backend.Convex_fill.draw t.g path.V.fill_first path.V.fill_count;
      if t.antialias then
        Backend.Convex_fill.draw_aa t.g path.V.stroke_first path.V.stroke_count
    | PFill b ->
      (* Render stencil *)
      Backend.Fill.prepare_stencil t.g;
      List.iter
        (fun {V. fill_first; fill_count} ->
           Backend.Fill.draw_stencil t.g fill_first fill_count)
        b.paths;
      Backend.Fill.prepare_cover t.g Texture.tex paint frame;
      if t.antialias then (
        (* Draw anti-aliased pixels *)
        Backend.Fill.prepare_aa t.g;
        List.iter
          (fun {V. stroke_first; stroke_count} ->
             Backend.Fill.draw_aa t.g stroke_first stroke_count)
          b.paths;
      );
      (* Cover *)
      Backend.Fill.finish_and_cover t.g b.triangle_offset b.triangle_count
    | PStroke (b, width) when t.stencil_strokes ->
      (* Fill the stroke base without overlap *)
      Backend.Stencil_stroke.prepare_stencil t.g Texture.tex paint frame width;
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Stencil_stroke.draw_stencil t.g stroke_first stroke_count)
        b.paths;
      (* Draw anti-aliased pixels. *)
      Backend.Stencil_stroke.prepare_aa
        t.g Texture.tex paint frame width;
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Stencil_stroke.draw_aa t.g stroke_first stroke_count)
        b.paths;
      (*  Clear stencil buffer. *)
      Backend.Stencil_stroke.prepare_clear t.g;
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Stencil_stroke.draw_clear t.g stroke_first stroke_count)
        b.paths;
      Backend.Stencil_stroke.finish t.g
    | PStroke (b, width) ->
      (*  Draw Strokes *)
      Backend.Direct_stroke.prepare t.g Texture.tex paint frame width;
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Direct_stroke.draw t.g stroke_first stroke_count)
        b.paths
    | PString (b, tex) ->
      Backend.Triangles.prepare t.g Texture.tex
        {paint with texture = Some tex} frame;
      Backend.Triangles.draw t.g b.triangle_offset b.triangle_count
    | PEmpty -> ()
    (* Recursive cases *)
    | PXform (n, xf)    ->
      xform_outofdate := true;
      exec t xf paint frame n;
      xform_outofdate := true
    | PPaint (n, paint) ->
      exec t xf paint frame n
    | PScissor (n, xf', box, `Set) ->
      let x = Gg.Box2.minx box in
      let y = Gg.Box2.miny box in
      let w = Gg.Box2.w box in
      let h = Gg.Box2.h box in
      exec t xf paint (Frame.set_scissor ~x ~y ~w ~h (Transform.compose xf' xf) frame) n
    | PScissor (n, xf', box, `Intersect) ->
      let x = Gg.Box2.minx box in
      let y = Gg.Box2.miny box in
      let w = Gg.Box2.w box in
      let h = Gg.Box2.h box in
      exec t xf paint (Frame.intersect_scissor ~x ~y ~w ~h (Transform.compose xf' xf) frame) n
    | PScissor  (n, _, _, `Reset) ->
      exec t xf paint (Frame.reset_scissor frame) n
    | PSeq (n1, n2) ->
      exec t xf paint frame n1;
      exec t xf paint frame n2
    | PAlpha (n, alpha) ->
      exec t xf paint {frame with Frame.alpha} n

  let render t ?performance_counter ~width ~height node =
    T.clear t.t;
    begin
      let {b; b'; _} = t in
      t.b <- b';
      t.b' <- b;
      t.b'_mark <- t.b_mark;
    end;
    B.clear t.b;
    t.b_mark <- ref ();
    let time0 = Backend.time_spent () and mem0 = Backend.memory_spent () in
    let todo = typesetter_prepare t [] 1.0 0.0 0.0 1.0 node in
    List.iter (fun f -> f ()) todo;
    let pnode = prepare t Transform.identity node in
    Backend.prepare t.g width height t.b.data (B.offset t.b);
    let time1 = Backend.time_spent () and mem1 = Backend.memory_spent () in
    xform_outofdate := true;
    exec t Transform.identity Paint.black Frame.default pnode;
    Backend.finish t.g;
    let time2 = Backend.time_spent () and mem2 = Backend.memory_spent () in
    begin match performance_counter with
      | None -> ()
      | Some pc ->
        Performance_counter.bump pc
          ~prep_time:(time1-time0) ~prep_mem:(mem1-mem0)
          ~flush_time:(time2-time1) ~flush_mem:(mem2-mem1)
    end
end

type color     = Color.t
type transform = Transform.t
type outline   = Outline.t
type path      = Path.t
type texture   = Texture.t
type image     = Image.t
type 'texture paint = 'texture Paint.t
type 'input typesetter = 'input Typesetter.t
