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
open Wall_geom

(* Length proportional to radius of a cubic bezier handle for 90deg arcs. *)
let kappa90 = 0.5522847493
let pi = 3.14159265358979323846264338327

type t = {
  t : T.t;
  b : B.t;
  g : Wall_gl.t;
  mutable xf : Transform.t;
  mutable p : Wall_gl.obj list;
}

let create_gl ~antialias = {
  t = T.make ();
  b = B.make ();
  g = Wall_gl.create ~antialias:true ~stencil_strokes:true ~debug:false;
  xf = Transform.identity;
  p = [];
}

let delete t =
  T.clear t.t;
  B.clear t.b;
  Wall_gl.delete t.g

let set_winding t w =
  T.set_winding t.t (match w with `CW -> T.CW | `CCW -> T.CCW)

let new_path t xf =
  T.clear t.t;
  t.xf <- xf

let close_path t =
  T.close_path t.t

let stroke t ?(frame=Frame.default) p o =
  let _bounds, paths = T.flush t.t in
  let paths =
    V.stroke t.t t.b
      ~edge_antialias:true
      ~fringe_width:1.0
      ~stroke_width:o.Outline.stroke_width
      ~miter_limit:o.Outline.miter_limit
      ~line_join:o.Outline.line_join
      ~line_cap:o.Outline.line_cap
      paths
  in
  t.p <- Wall_gl.Stroke (p, frame, o.Outline.stroke_width, paths) :: t.p

let fill t ?(frame=Frame.default) p =
  let bounds, paths = T.flush t.t in
  let paths =
    V.fill t.t t.b
      ~edge_antialias:true
      ~fringe_width:1.0
      paths
  in
  t.p <- Wall_gl.Fill (p, frame, bounds, paths) :: t.p

let new_frame t =
  T.clear t.t;
  B.clear t.b;
  t.p <- []

let flush_frame t sz =
  Wall_gl.render t.g sz t.b (List.rev t.p)

let move_to t ~x ~y =
  T.move_to t.t
    (Transform.px t.xf x y)
    (Transform.py t.xf x y)

let line_to t ~x ~y =
  T.line_to t.t
    (Transform.px t.xf x y)
    (Transform.py t.xf x y)

let bezier_to t ~c1x ~c1y ~c2x ~c2y ~x ~y =
  T.bezier_to t.t
    ~x1:(Transform.px t.xf c1x c1y)
    ~y1:(Transform.py t.xf c1x c1y)
    ~x2:(Transform.px t.xf c2x c2y)
    ~y2:(Transform.py t.xf c2x c2y)
    ~x3:(Transform.px t.xf x y)
    ~y3:(Transform.py t.xf x y)

let quad_to t ~cx ~cy ~x ~y =
  let x0 = T.last_x t.t in
  let y0 = T.last_y t.t in
  let cx = Transform.px t.xf cx cy in
  let cy = Transform.py t.xf cx cy in
  let x = Transform.px t.xf x y in
  let y = Transform.py t.xf x y in
  T.bezier_to t.t
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
  close_path t

let round_rect t ~x ~y ~w ~h ~r =
  if r < 0.1 then
    rect t ~x ~y ~w ~h
  else begin
    let rx = copysign (min r (abs_float w *. 0.5)) w in
    let ry = copysign (min r (abs_float h *. 0.5)) h in
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
    close_path t
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
  close_path t

let circle t ~cx ~cy ~r =
  ellipse t ~cx ~cy ~rx:r ~ry:r

let arc t ~cx ~cy ~r ~a0 ~a1 dir =
  let da = (a1 -. a0) in
  let da =
    if abs_float da > 2.0 *. pi then
      match dir with
      | `CW  -> 2.0 *. pi
      | `CCW -> -. 2.0 *. pi
    else
      match dir with
      | `CW  -> if da < 0.0 then da +. 2.0 *. pi else da
      | `CCW -> if da > 0.0 then da -. 2.0 *. pi else da
  in
  let ndivs = max 1 (min 5 (int_of_float (abs_float da /. pi *. 0.5 +. 0.5))) in
  let hda = (da /. float ndivs) /. 2.0 in
  (* Split arc into max 90 degree segments. *)
  let kappa = abs_float (4.0 /. 3.0 *. (1.0 -. cos hda) /. sin hda) in
  let kappa = match dir with
    | `CW  -> kappa
    | `CCW -> kappa
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
  if T.has_path t.t then
    line_to t ~x ~y
  else
    move_to t ~x ~y;
  step coords 1

(*let arc_to vg xf ~x1 ~y1 ~x2 ~y2 ~r =
{
	float x0 = ctx->commandx;
	float y0 = ctx->commandy;
	float dx0,dy0, dx1,dy1, a, d, cx,cy, a0,a1;
	int dir;

	if (ctx->ncommands == 0) {
		return;
	}

	// Handle degenerate cases.
	if (nvg__ptEquals(x0,y0, x1,y1, ctx->distTol) ||
		nvg__ptEquals(x1,y1, x2,y2, ctx->distTol) ||
		nvg__distPtSeg(x1,y1, x0,y0, x2,y2) < ctx->distTol*ctx->distTol ||
		radius < ctx->distTol) {
		nvgLineTo(ctx, x1,y1);
		return;
	}

	// Calculate tangential circle to lines (x0,y0)-(x1,y1) and (x1,y1)-(x2,y2).
	dx0 = x0-x1;
	dy0 = y0-y1;
	dx1 = x2-x1;
	dy1 = y2-y1;
	nvg__normalize(&dx0,&dy0);
	nvg__normalize(&dx1,&dy1);
	a = nvg__acosf(dx0*dx1 + dy0*dy1);
	d = radius / nvg__tanf(a/2.0f);

//	printf("a=%f° d=%f\n", a/NVG_PI*180.0f, d);

	if (d > 10000.0f) {
		nvgLineTo(ctx, x1,y1);
		return;
	}

	if (nvg__cross(dx0,dy0, dx1,dy1) > 0.0f) {
		cx = x1 + dx0*d + dy0*radius;
		cy = y1 + dy0*d + -dx0*radius;
		a0 = nvg__atan2f(dx0, -dy0);
		a1 = nvg__atan2f(-dx1, dy1);
		dir = NVG_CW;
//		printf("CW c=(%f, %f) a0=%f° a1=%f°\n", cx, cy, a0/NVG_PI*180.0f, a1/NVG_PI*180.0f);
	} else {
		cx = x1 + dx0*d + -dy0*radius;
		cy = y1 + dy0*d + dx0*radius;
		a0 = nvg__atan2f(-dx0, dy0);
		a1 = nvg__atan2f(dx1, -dy1);
		dir = NVG_CCW;
//		printf("CCW c=(%f, %f) a0=%f° a1=%f°\n", cx, cy, a0/NVG_PI*180.0f, a1/NVG_PI*180.0f);
	}

	nvgArc(ctx, cx, cy, radius, a0, a1, dir);
  }*)
