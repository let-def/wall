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
open Wall
open Wall_geom
module Backend = Wall_backend_c

type t = {
  backend: Backend.t;
  antialias : bool;
  stencil_strokes: bool;
  debug : bool;
}

let antialias t = t.antialias

let create
    ~antialias
    ~stencil_strokes
    ~debug
  =
  { backend = Backend.create ~antialias;
    antialias; stencil_strokes; debug; }

let delete t =
  Backend.delete t.backend;

type 'a typesetter = {
  allocate: transform -> 'a -> unit;
  bake: transform -> 'a -> unit;
  render: transform -> x:float -> y:float -> 'a -> (Stb_truetype.char_quad -> unit) -> Wall_tex.t option;
}

let typesetter ~allocate ~bake ~render =
  { allocate; bake; render }

type obj =
  | Fill   of transform * Wall_tex.t paint * frame * T.bounds * V.path list
  | Stroke of transform * Wall_tex.t paint * frame * float * V.path list
  | String :  transform * unit paint * frame * float * float * 'a typesetter * 'a -> obj

type kind =
  | FILL
  | CONVEXFILL
  | STROKE
  | TRIANGLES

type call = {
  kind  : kind;
  frame : frame;
  xform : transform;
  paint : Wall_tex.t paint;
  width : float;
  paths : V.path list;
  triangle_offset : int;
  triangle_count  : int;
}

let push_4 b f0 f1 f2 f3 =
  let data = B.data b and c = B.alloc b 4 in
  data.{c + 0} <- f0;
  data.{c + 1} <- f1;
  data.{c + 2} <- f2;
  data.{c + 3} <- f3

let prepare_fill vb xform paint frame bounds paths =
  let convex = match paths with
    | [path] -> path.V.convex
    | _ -> false
  in
  if convex then (
    { kind = CONVEXFILL; paths; width = 1.0; xform;
      triangle_offset = 0; triangle_count = 0; paint; frame }
  ) else (
    let {T. minx; miny; maxx; maxy} = bounds in
    B.reserve vb (4 * 4);
    let triangle_offset = B.offset vb / 4 in
    push_4 vb maxx maxy 0.5 1.0;
    push_4 vb maxx miny 0.5 1.0;
    push_4 vb minx maxy 0.5 1.0;
    push_4 vb minx miny 0.5 1.0;
    { kind = FILL; paths; width = 1.0; xform;
      triangle_offset; triangle_count = 4; paint; frame }
  )

let prepare_stroke xform paint frame width paths =
  { kind = STROKE; xform; paint; frame; width; paths;
    triangle_offset = 0; triangle_count = 6 }

let prepare_string vbuffer xform paint frame x y typesetter text =
  let offset = B.offset vbuffer in
  match typesetter.render xform ~x ~y text
          (fun q ->
             let open Stb_truetype in
             B.reserve vbuffer (6 * 4);
             push_4 vbuffer q.bx0 q.by0 q.s0 q.t0;
             push_4 vbuffer q.bx1 q.by1 q.s1 q.t1;
             push_4 vbuffer q.bx1 q.by0 q.s1 q.t0;
             push_4 vbuffer q.bx0 q.by0 q.s0 q.t0;
             push_4 vbuffer q.bx0 q.by1 q.s0 q.t1;
             push_4 vbuffer q.bx1 q.by1 q.s1 q.t1
          )
  with
  | None ->
    { kind = TRIANGLES; frame = Frame.default; paint = Paint.black;
      width = 1.0; paths = []; xform = Transform.identity;
      triangle_offset = 0; triangle_count = 0; }
  | Some texture ->
    let paint = {paint with Paint.image = Some texture} in
    { kind = TRIANGLES; frame; paint; width = 1.0; paths = []; xform;
      triangle_offset = offset / 4;
      triangle_count  = (B.offset vbuffer - offset) / 4;
    }

let prepare_obj t vbuffer = function
  | Fill (xform, paint, frame, bounds, paths) ->
    prepare_fill vbuffer xform paint frame bounds paths
  | Stroke (xform, paint, frame, width, paths) ->
    prepare_stroke xform paint frame (width *. Transform.average_scale xform) paths
  | String (xform, paint, frame, x, y, typesetter, text) ->
    prepare_string vbuffer xform paint frame x y typesetter text

let exec_call t cmd =
  Backend.set_reversed cmd.xform;
  match cmd.kind with
  | FILL ->
    (* Render stencil *)
    Backend.Fill.prepare_stencil t.backend cmd.xform;
    List.iter
      (fun {V. fill_first; fill_count} ->
         Backend.Fill.draw_stencil fill_first fill_count)
      cmd.paths;
    Backend.Fill.prepare_cover t.backend cmd.paint cmd.frame cmd.width;
    if t.antialias then (
      (* Draw anti-aliased pixels *)
      Backend.Fill.prepare_aa ();
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Fill.draw_aa stroke_first stroke_count)
        cmd.paths;
    );
    (* Cover *)
    Backend.Fill.finish_and_cover cmd.triangle_offset cmd.triangle_count
  | CONVEXFILL ->
    Backend.Convex_fill.prepare t.backend cmd.xform cmd.paint cmd.frame cmd.width;
    List.iter
      (fun {V. fill_first; fill_count} ->
         Backend.Convex_fill.draw fill_first fill_count)
      cmd.paths;
    if t.antialias then (
      List.iter
        (fun {V. stroke_first; stroke_count} ->
           Backend.Convex_fill.draw_aa stroke_first stroke_count)
        cmd.paths
    )
  | STROKE when t.stencil_strokes ->
    (* Fill the stroke base without overlap *)
    Backend.Stencil_stroke.prepare_stencil
      t.backend cmd.xform cmd.paint cmd.frame cmd.width;
    List.iter
      (fun {V. stroke_first; stroke_count} ->
         Backend.Stencil_stroke.draw_stencil stroke_first stroke_count)
      cmd.paths;
    (* Draw anti-aliased pixels. *)
    Backend.Stencil_stroke.prepare_aa t.backend cmd.paint cmd.frame cmd.width;
    List.iter
      (fun {V. stroke_first; stroke_count} ->
         Backend.Stencil_stroke.draw_aa stroke_first stroke_count)
      cmd.paths;
    (*  Clear stencil buffer. *)
    Backend.Stencil_stroke.prepare_clear ();
    List.iter
      (fun {V. stroke_first; stroke_count} ->
         Backend.Stencil_stroke.draw_clear stroke_first stroke_count)
      cmd.paths;
    Backend.Stencil_stroke.finish ()
  | STROKE ->
    (*  Draw Strokes *)
    Backend.Direct_stroke.prepare
      t.backend cmd.xform cmd.paint cmd.frame cmd.width;
    List.iter
      (fun {V. stroke_first; stroke_count} ->
         Backend.Direct_stroke.draw stroke_first stroke_count)
      cmd.paths
  | TRIANGLES ->
    Backend.Triangles.prepare t.backend cmd.xform cmd.paint cmd.frame;
    Backend.Triangles.draw cmd.triangle_offset cmd.triangle_count

let render t viewsize vbuffer objs =
  List.iter (function
      | String (xf, _, _, _, _, typesetter, text) ->
        typesetter.allocate xf text
      | _ -> ()
    ) objs;
  List.iter (function
      | String (xf, _, _, _, _, typesetter, text) ->
        typesetter.bake xf text
      | _ -> ()
    ) objs;
  let calls = List.map (prepare_obj t vbuffer) objs in
  Backend.prepare t.backend
    (Size2.w viewsize) (Size2.h viewsize) (B.sub vbuffer);
  List.iter (exec_call t) calls;
  Backend.finish ()
