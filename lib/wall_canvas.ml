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

  let round_rect t ~x ~y ~w ~h ~r =
    if r < 0.1 then
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

type group_kind = Partial | Total | Partial_after | Total_after

type order = [ `Partial | `Total ]

type t = {
  t : T.t;
  b : B.t;
  g : Wall_gl.t;
  mutable size : Gg.size2;
  mutable task : task;
}

and task =
  | Node  of {mutable sibling : task; mutable child : task; prepare : t -> Wall_gl.obj}
  | Group of {mutable sibling : task; mutable child : task; kind: group_kind}
  | Leaf
  | Done

let create_gl ?(antialias=true) ?(stencil_strokes=true) () = {
  t = T.make ();
  b = B.make ();
  g = Wall_gl.create ~antialias ~stencil_strokes ~debug:false;
  size = Gg.Size2.unit;
  task = Done;
}

let delete t =
  T.clear t.t;
  B.clear t.b;
  Wall_gl.delete t.g

let prepare_path t ~quality xf path =
  T.clear t.t;
  T.set_tess_tol t.t (0.25 /. (Transform.average_scale xf *. quality));
  path.closure t.t

type shape = frame:frame -> quality:float -> transform -> Wall_tex.t paint -> t -> Wall_gl.obj

let stroke {Outline. stroke_width; miter_limit; line_join; line_cap} path : shape =
  fun ~frame ~quality xf paint t ->
    prepare_path t ~quality xf path;
    let _bounds, paths = T.flush t.t in
    let paths =
      V.stroke t.t t.b
        ~edge_antialias:true
        ~fringe_width:(1.0 /. Transform.average_scale xf)
        ~stroke_width
        ~miter_limit
        ~line_join
        ~line_cap
        paths
    in
    Wall_gl.Stroke (xf, Paint.transform paint xf, frame, stroke_width, paths)

let fill path : shape =
  fun ~frame ~quality xf paint t ->
    prepare_path t ~quality xf path;
    let bounds, paths = T.flush t.t in
    let paths =
      V.fill t.t t.b
        ~edge_antialias:true
        ~fringe_width:(1.0 /. Transform.average_scale xf)
        paths
    in
    Wall_gl.Fill (xf, Paint.transform paint xf, frame, bounds, paths)

let task_is_done = function
  | Node {child=Done} | Group {child=Done} | Done -> true
  | _ -> false

let task_add prepare = function
  | Leaf -> assert false
  | Node {sibling=Done} | Group {sibling=Done} | Done ->
    invalid_arg "Wall_canvas.task: frame already flushed"
  | Node n ->
    let node = Node {sibling = n.child; child=Leaf; prepare} in
    n.child <- node; node
  | Group n ->
    let node = Node {sibling = n.child; child=Leaf; prepare} in
    n.child <- node; node

let task_mark_done task =
  let rec loop = function
    | Done | Leaf -> ()
    | Node n ->
      let child = n.child in
      n.child <- Done;
      loop child;
      let next = n.sibling in
      n.sibling <- Done;
      loop next
    | Group n ->
      let child = n.child in
      n.child <- Done;
      loop child;
      let next = n.sibling in
      n.sibling <- Done;
      loop next
  in
  loop task

let task_linearize t task =
  let rec aux t acc after = function
    | Done -> failwith "Wall_canvas.flush: frame already flushed"
    | Leaf -> begin match after with
        | [] -> acc
        | x :: xs -> aux t acc xs x
      end
    | Node n ->
      let child = n.child and next = n.sibling in
      n.child <- Done;
      n.sibling <- Done;
      aux t (n.prepare t :: aux t acc [] child) after next
    | Group ({kind = Partial | Total} as n) ->
      let child = n.child and next = n.sibling in
      n.child <- Done;
      n.sibling <- Done;
      aux t (aux t acc [] child) after next
    | Group ({kind = Partial_after | Total_after} as n) ->
      let child = n.child and next = n.sibling in
      n.child <- Done;
      n.sibling <- Done;
      aux t acc (child :: after) next
  in
  aux t [] [] task

let group_kind order after =
  match order, after with
  | `Partial, true -> Partial_after
  | `Partial, false -> Partial
  | `Total, true -> Total_after
  | `Total, false -> Total

let new_frame ?(order=`Partial) t sz =
  T.clear t.t;
  B.clear t.b;
  task_mark_done t.task;
  let node = Group {kind = group_kind order false; sibling = Leaf; child = Leaf} in
  t.size <- sz;
  t.task <- node;
  node

let flush_frame t =
  let task = t.task in
  t.task <- Done;
  Wall_gl.render t.g t.size t.b (task_linearize t task)

let draw task ?(frame=Frame.default) ?(quality=1.0) xf paint (shape : shape) =
  task_add (shape ~frame ~quality xf paint) task

let text task ?(frame=Frame.default) ?(halign=`LEFT) ?(valign=`BASELINE) xf paint font ~x ~y str =
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
  let paint = Paint.transform paint xf in
  task_add (fun _ -> Wall_gl.Text (xf, paint, frame, x, y, font, str)) task

let group ?(order=`Partial) ?(after=false) = function
  | Leaf | Done -> assert false
  | Node {sibling=Done} | Group {sibling=Done} ->
    invalid_arg "Wall_canvas.task: frame already flushed"
  | Node n ->
    let kind = group_kind order after in
    let node = Group {sibling = n.child; child = Leaf; kind} in
    n.child <- node;
    node
  | Group n ->
    let kind = group_kind order after in
    let node = Group {sibling = n.child; child = Leaf; kind} in
    n.child <- node;
    node

(* Convenient definitions *)

let stroke_path outline f = stroke outline (path f)

let fill_path f = fill (path f)

let draw' task ?frame ?quality xf paint shape =
  ignore (draw task ?frame ?quality xf paint shape : task)

let text' task ?frame ?halign ?valign xf paint font ~x ~y str =
  ignore (text task ?frame ?halign ?valign xf paint font str ~x ~y)
