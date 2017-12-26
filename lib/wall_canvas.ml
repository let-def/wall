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

module Backend = Wall__backend

(* Length proportional to radius of a cubic bezier handle for 90deg arcs. *)
let kappa90 = 0.5522847493
let pi = 3.14159265358979323846264338327

let maxf x y : float = if x >= y then x else y
let minf x y : float = if x <= y then x else y
let maxi x y : int   = if x >= y then x else y
let mini x y : int   = if x <= y then x else y

module Path = struct
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
        let rx = copysign (minf rbl hw) w in
        let ry = copysign (minf rbl hh) h in
        move_to t ~x ~y:(y +. h -. ry);
        bezier_to t
          ~c1x:x ~c1y:(y +. h -. ry *. (1.0 -. kappa90))
          ~c2x:(x +. rx *. (1.0 -. kappa90)) ~c2y:(y +. h)
          ~x:(x +. rx) ~y:(y +. h);
      end;
      begin
        let rx = copysign (minf rbr hw) w in
        let ry = copysign (minf rbr hh) h in
        line_to t ~x:(x +. w -. rx) ~y:(y +. h);
        bezier_to t
          ~c1x:(x +. w -. rx *. (1.0 -. kappa90)) ~c1y:(y +. h)
          ~c2x:(x +. w) ~c2y:(y +. h -. ry *. (1.0 -. kappa90))
          ~x:(x +. w) ~y:(y +. h -. ry);
      end;
      begin
        let rx = copysign (minf rtr hw) w in
        let ry = copysign (minf rtr hh) h in
        line_to t ~x:(x +. w) ~y:(y +. ry);
        bezier_to t
          ~c1x:(x +. w) ~c1y:(y +. ry *. (1.0 -. kappa90))
          ~c2x:(x +. w -. rx *. (1.0 -. kappa90)) ~c2y:y
          ~x:(x +. w -. rx) ~y;
      end;
      begin
        let rx = copysign (minf rtl hw) w in
        let ry = copysign (minf rtl hh) h in
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
      let rx = copysign (minf r (abs_float w *. 0.5)) w in
      let ry = copysign (minf r (abs_float h *. 0.5)) h in
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
    let ndivs = maxi 1 (mini 5 (int_of_float (abs_float da /. (pi *. 0.5) +. 0.5))) in
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
end

type path = {
  closure : (Path.ctx -> unit);
  cache_key : unit;
}

let path closure = { closure; cache_key = () }

type t = {
  t : T.t;
  b : B.t;
  g : Wall__backend.t;
  antialias : bool;
  stencil_strokes : bool;
}

let create ?(antialias=true) ?(stencil_strokes=true) () = {
  t = T.make ();
  b = B.make ();
  g = Wall__backend.create ~antialias;
  antialias;
  stencil_strokes;
}

let delete t =
  T.clear t.t;
  B.clear t.b;
  Backend.delete t.g

let prepare_path t ~quality xf path =
  T.clear t.t;
  let factor =
   let {Transform. x00; x10; x01; x11; _} = xf in
    let sx = x00 *. x00 +. x10 *. x10 in
    let sy = x01 *. x01 +. x11 *. x11 in
    sx *. sy
  in
  T.set_tess_tol t.t (0.25 /. (factor *. quality));
  path.closure t.t

module Render = struct

  type node =
    (* Base cases *)
    | None
    | Fill   of path
    | Stroke of path * outline
    | String :  'a * ('a, Wall_tex.t) typesetter -> node
    (* Recursive cases *)
    | Xform  of node * transform
    | Paint  of node * Wall_tex.t paint
    | Scissor of node * Gg.box2 * [`Set | `Reset | `Intersect]
    | Alpha  of node * float
    | Seq    of node * node

  let rec typesetter_prepare acc xx xy yx yy = function
    | None | Fill _ | Stroke _ -> acc
    | Paint (n, _) | Alpha (n, _) | Scissor (n, _, _) ->
      typesetter_prepare acc xx xy yx yy n
    | Seq (n1, n2) ->
      let acc = typesetter_prepare acc xx xy yx yy n1 in
      typesetter_prepare acc xx xy yx yy n2
    | Xform (n, xf) ->
      typesetter_prepare acc
        (Transform.px xf xx xy) (Transform.py xf xx xy)
        (Transform.px xf yx yy) (Transform.py xf yx yy)
        n
    | String (x, cls) ->
      let sx = sqrt (xx *. xx +. xy *. xy) in
      let sy = sqrt (yx *. yx +. yy *. yy) in
      match cls.allocate ~sx ~sy x with
      | None -> acc
      | Some f -> (f :: acc)

  type buffer_item = {
    paths : V.path list;
    triangle_offset : int;
    triangle_count  : int;
  }

  type prepared_node =
    (* Base cases *)
    | PFill   of buffer_item
    | PStroke of buffer_item * float
    | PString of buffer_item * Wall_tex.t
    | PNone
    (* Recursive cases *)
    | PXform  of prepared_node * transform
    | PPaint  of prepared_node * Wall_tex.t paint
    | PScissor of prepared_node * Gg.box2 * [`Set | `Reset | `Intersect]
    | PAlpha  of prepared_node * float
    | PSeq    of prepared_node * prepared_node

  let is_convex = function
    | [path] -> path.V.convex
    | _ -> false

  let quadbuf =
    {Typesetter.
      x0 = 0.0; y0 = 0.0; x1 = 0.0; y1 = 0.0;
      u0 = 0.0; v0 = 0.0; u1 = 0.0; v1 = 0.0}

  let push_quad b =
    let d = B.data b and c = B.alloc b (6 * 4) in
    let q = quadbuf in
    d.{c+ 0+0}<-q.x0; d.{c+ 0+1}<-q.y0; d.{c+ 0+2}<-q.u0; d.{c+ 0+3}<-q.v0;
    d.{c+ 4+0}<-q.x1; d.{c+ 4+1}<-q.y1; d.{c+ 4+2}<-q.u1; d.{c+ 4+3}<-q.v1;
    d.{c+ 8+0}<-q.x1; d.{c+ 8+1}<-q.y0; d.{c+ 8+2}<-q.u1; d.{c+ 8+3}<-q.v0;
    d.{c+12+0}<-q.x0; d.{c+12+1}<-q.y0; d.{c+12+2}<-q.u0; d.{c+12+3}<-q.v0;
    d.{c+16+0}<-q.x0; d.{c+16+1}<-q.y1; d.{c+16+2}<-q.u0; d.{c+16+3}<-q.v1;
    d.{c+20+0}<-q.x1; d.{c+20+1}<-q.y1; d.{c+20+2}<-q.u1; d.{c+20+3}<-q.v1

  let push_quad_strip b =
    let d = B.data b and c = B.alloc b (4 * 4) in
    let q = quadbuf in
    d.{c+ 0+0}<-q.x1; d.{c+ 0+1}<-q.y1; d.{c+ 0+2}<-q.u1; d.{c+ 0+3}<-q.v1;
    d.{c+ 4+0}<-q.x1; d.{c+ 4+1}<-q.y0; d.{c+ 4+2}<-q.u1; d.{c+ 4+3}<-q.v0;
    d.{c+ 8+0}<-q.x0; d.{c+ 8+1}<-q.y1; d.{c+ 8+2}<-q.u0; d.{c+ 8+3}<-q.v1;
    d.{c+12+0}<-q.x0; d.{c+12+1}<-q.y0; d.{c+12+2}<-q.u0; d.{c+12+3}<-q.v0

  let rec prepare t xf = function
    (* Base cases *)
    | None -> PNone
    | Fill path ->
      prepare_path t ~quality:1.0 xf path;
      let bounds, paths = T.flush t.t in
      let paths =
        V.fill t.t t.b
          ~edge_antialias:t.antialias
          ~fringe_width:(1.0 /. Transform.average_scale xf)
          paths
      in
      if is_convex paths then (
        PFill { paths; triangle_offset = 0; triangle_count = 0 }
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
        PFill { paths; triangle_offset; triangle_count = 4 }
      )
    | Stroke (path, {Outline. stroke_width; miter_limit; line_join; line_cap}) ->
      prepare_path t ~quality:1.0 xf path;
      let _bounds, paths = T.flush t.t in
      let paths =
        V.stroke t.t t.b
          ~edge_antialias:t.antialias
          ~fringe_width:(1.0 /. Transform.average_scale xf)
          ~stroke_width
          ~miter_limit
          ~line_join
          ~line_cap
          paths
      in
      PStroke ({ paths; triangle_offset = 0; triangle_count = 6 }, stroke_width)
    | String (x, cls) ->
      let vbuffer = t.b in
      let offset = B.offset vbuffer in
      begin match cls.Typesetter.render xf x quadbuf
                    (fun () ->
                       B.reserve vbuffer (6 * 4);
                       push_quad vbuffer)
        with
        | exception _ -> PNone
        | texture ->
          let triangle_offset = offset / 4 in
          let triangle_count  = (B.offset vbuffer - offset) / 4 in
          PString ({ paths = []; triangle_offset; triangle_count; }, texture)
      end
    (* Recursive cases *)
    | Xform  (n, xf') ->
      let xf =
        if xf == Transform.identity then xf'
        else Transform.compose xf' xf
      in
      PXform (prepare t xf n, xf)
    | Paint  (n, p) ->
      PPaint (prepare t xf n, Paint.transform p xf)
    | Scissor (n, box, action) ->
      PScissor (prepare t xf n, box, action)
    | Alpha (n, a) ->
      PAlpha (prepare t xf n, a)
    | Seq (n1, n2) ->
      PSeq (prepare t xf n1, prepare t xf n2)

  let xform_outofdate = ref true

  let counter_fill = ref 0
  let counter_convex_fill = ref 0
  let counter_stroke = ref 0
  let counter_opaque = ref 0
  let counter_transparent = ref 0

  let rec exec t xf paint frame = function
    | PFill _ | PStroke _ | PString _ when
        !xform_outofdate &&
        (Backend.set_xform t.g xf; xform_outofdate := false; false) -> assert false
    | PFill { paths = [path]; triangle_offset = 0; triangle_count = 0 } ->
      incr counter_convex_fill;
      Backend.Convex_fill.prepare t.g Wall_tex.tex paint frame;
      Backend.Convex_fill.draw path.V.fill_first path.V.fill_count;
      if t.antialias then
        Backend.Convex_fill.draw_aa path.V.stroke_first path.V.stroke_count
    | PFill b ->
      incr counter_fill;
      (* Render stencil *)
      Backend.Fill.prepare_stencil t.g;
      List.iter
        (fun {V. fill_first; fill_count} ->
           Backend.Fill.draw_stencil fill_first fill_count)
        b.paths;
      Backend.Fill.prepare_cover t.g Wall_tex.tex paint frame;
      if t.antialias then (
        (* Draw anti-aliased pixels *)
        Backend.Fill.prepare_aa ();
        List.iter
          (fun {V. stroke_first; stroke_count} ->
             Backend.Fill.draw_aa stroke_first stroke_count)
          b.paths;
      );
      (* Cover *)
      Backend.Fill.finish_and_cover b.triangle_offset b.triangle_count
    | PStroke (b, width) when t.stencil_strokes ->
      (* Fill the stroke base without overlap *)
      incr counter_stroke;
      Backend.Stencil_stroke.prepare_stencil t.g Wall_tex.tex paint frame width;
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Stencil_stroke.draw_stencil stroke_first stroke_count)
        b.paths;
      (* Draw anti-aliased pixels. *)
      Backend.Stencil_stroke.prepare_aa
        t.g Wall_tex.tex paint frame width;
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Stencil_stroke.draw_aa stroke_first stroke_count)
        b.paths;
      (*  Clear stencil buffer. *)
      Backend.Stencil_stroke.prepare_clear ();
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Stencil_stroke.draw_clear stroke_first stroke_count)
        b.paths;
      Backend.Stencil_stroke.finish ()
    | PStroke (b, width) ->
      incr counter_stroke;
      (*  Draw Strokes *)
      Backend.Direct_stroke.prepare t.g Wall_tex.tex paint frame width;
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Direct_stroke.draw stroke_first stroke_count)
        b.paths
    | PString (b, tex) ->
      Backend.Triangles.prepare t.g Wall_tex.tex
        {paint with Paint.image = Some tex} frame;
      Backend.Triangles.draw b.triangle_offset b.triangle_count
    | PNone -> ()
    (* Recursive cases *)
    | PXform (n, xf)    ->
      xform_outofdate := true;
      exec t xf paint frame n;
      xform_outofdate := true
    | PPaint (n, paint) ->
      if Color.a paint.Paint.inner < 1.0 || Color.a paint.Paint.outer < 1.0 then
        incr counter_transparent
      else
        incr counter_opaque;
      exec t xf paint frame n
    | PScissor (n, box, `Set) ->
      let x = Gg.Box2.minx box in
      let y = Gg.Box2.miny box in
      let w = Gg.Box2.w box in
      let h = Gg.Box2.h box in
      exec t xf paint (Wall.Frame.set_scissor ~x ~y ~w ~h xf frame) n
    | PScissor (n, box, `Intersect) ->
      let x = Gg.Box2.minx box in
      let y = Gg.Box2.miny box in
      let w = Gg.Box2.w box in
      let h = Gg.Box2.h box in
      exec t xf paint (Wall.Frame.intersect_scissor ~x ~y ~w ~h xf frame) n
    | PScissor  (n, _, `Reset) ->
      exec t xf paint (Wall.Frame.reset_scissor frame) n
    | PSeq (n1, n2) ->
      exec t xf paint frame n1;
      exec t xf paint frame n2
    | PAlpha (n, alpha) ->
      exec t xf paint {frame with Frame.alpha} n

  let render t ~width ~height node =
    let time0 = Backend.time_spent () and mem0 = Backend.memory_spent () in
    let todo = typesetter_prepare [] 1.0 0.0 0.0 1.0 node in
    let time1 = Backend.time_spent () and mem1 = Backend.memory_spent () in
    List.iter (fun f -> f ()) todo;
    let time2 = Backend.time_spent () and mem2 = Backend.memory_spent () in
    (*for i = 0 to 99 do ignore (prepare t Transform.identity node) done;*)
    let pnode = prepare t Transform.identity node in
    let time3 = Backend.time_spent () and mem3 = Backend.memory_spent () in
    Backend.prepare t.g width height (B.sub t.b);
    xform_outofdate := true;
    counter_fill := 0;
    counter_convex_fill := 0;
    counter_stroke := 0;
    counter_transparent := 0;
    counter_opaque := 0;
    exec t Transform.identity Paint.black Frame.default pnode;
    Backend.finish ();
    let time4 = Backend.time_spent () and mem4 = Backend.memory_spent () in
    let row name t0 t1 m0 m1 =
      Printf.printf "% 9.03f us % 9d words     %s\n" (float (t1 - t0) /. 1000.0) (m1 - m0) name
    in
    Printf.printf "--- new frame: %d convex fill, %d complex fill, %d stroke, %d transparent styles, %d opaque styles\n" !counter_convex_fill !counter_fill !counter_stroke !counter_transparent !counter_opaque;
    row "typeset preparation" time0 time1 mem0 mem1;
    row (Printf.sprintf "typeset baking (%d jobs)" (List.length todo)) time1 time2 mem1 mem2;
    row "command list preparation" time2 time3 mem2 mem3;
    row "command list submission (GL driver)" time3 time4 mem3 mem4;
    Printf.printf "%!"
end

(* Nodes *)

type node = Render.node

let stroke outline path : node = Render.Stroke (path, outline)

let fill path = Render.Fill path

let typeset typesetter contents = Render.String (contents, typesetter)

(* Convenient definitions *)

let stroke_path outline f = stroke outline (path f)

let fill_path f = fill (path f)

let typesetter = Wall_glyph.typesetter ()

let simple_text ?(halign=`LEFT) ?(valign=`BASELINE) font ~x ~y str =
  let x = match halign with
    | `LEFT   -> x
    | `CENTER -> (x -. Font.text_width font str *. 0.5)
    | `RIGHT  -> (x -. Font.text_width font str)
  in
  let y = match valign with
    | `TOP    -> y +. (Font.font_metrics font).Font.ascent
    | `BASELINE -> y
    | `BOTTOM -> y +. (Font.font_metrics font).Font.descent
    | `MIDDLE ->
      let {Font. ascent; descent} = Font.font_metrics font in
      (y +. (ascent +. descent) *. 0.5)
  in
  typeset typesetter (font, Gg.P2.v x y, str)

let render t ~width ~height node =
  T.clear t.t;
  B.clear t.b;
  Render.render t ~width ~height node

let paint paint node = Render.Paint (node, paint)
let transform xf node = Render.Xform (node, xf)
let impose n1 n2 = Render.Seq (n1, n2)
let rec seq = function
  | [] -> Render.None
  | [x] -> x
  | x :: xs -> impose x (seq xs)
let none = Render.None
let scissor box node = Render.Scissor (node, box, `Set)
let reset_scissor node = Render.Scissor (node, Gg.Box2.empty, `Reset)
let intersect_scissor box node = Render.Scissor (node, box, `Intersect)
let alpha a node = Render.Alpha (node, a)
