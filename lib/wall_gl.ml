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

open Tgles2
open Gg
open Wall
open Wall_geom

type font_buffer = {
  image: Stb_image.int8 Stb_image.t;
  texture: Wall_tex.t;
  mutable room : unit Maxrects.t;
}

module Glyph = struct
  let quantize x = int_of_float (x *. 10.0)

  let estimate_scale xf {Font. size} =
    let factor = Transform.average_scale xf in
    let scale = factor *. size in
    (*Printf.eprintf "sx = %f, sy = %f, size = %f, scale = %f\n%!"
      sx sy size scale;*)
    let x = quantize scale in
    if x > 2000
    then (float x /. 2000.0 /. factor, 2000)
    else (1.0 /. factor, x)

  type key = {
    cp    : int;
    scale : int;
    ttf   : Stb_truetype.t;
    blur  : int;
  }

  let key xf font =
    let ttf = font.Font.glyphes in
    let blur = quantize font.Font.blur in
    let factor, scale = estimate_scale xf font in
    (factor, (fun cp -> { cp; scale; ttf; blur }))

  type cell = {
    box   : Stb_truetype.box;
    uv    : Stb_truetype.box;
    glyph : Stb_truetype.glyph;
    mutable frame : int;
  }
end

type t = {
  program   : int;
  viewsize  : int;
  viewxform : int;
  tex       : int;
  frag      : int;
  vert_vbo  : int;

  antialias : bool;
  stencil_strokes: bool;
  debug : bool;

  font_glyphes: (Glyph.key, Glyph.cell) Hashtbl.t;
  font_todo: (Glyph.key, unit) Hashtbl.t;
  mutable font_buffer: font_buffer option;
}

(* OpenGL rendered from
   https://github.com/memononen/nanovg/blob/master/src/nanovg_gl.h *)

let shader_vertex = "\
uniform vec2 viewSize;
uniform vec3 viewXform[3];
attribute vec2 vertex;
attribute vec2 tcoord;
varying vec2 ftcoord;
varying vec2 fpos;

void main(void) {
  ftcoord = tcoord;
  fpos = (mat3(viewXform[0], viewXform[1], viewXform[2]) * vec3(vertex,1.0)).xy;
  gl_Position = vec4(2.0 * fpos.x / viewSize.x - 1.0,
                     1.0 - 2.0 * fpos.y / viewSize.y, 0, 1);
}
"

let shader_fragment = "\

#define innerCol     frag[6]
#define outerCol     frag[7]
#define paintMat     mat3(frag[3].xyz, frag[4].xyz, frag[5].xyz)
#define scissorMat   mat3(frag[0].xyz, frag[1].xyz, frag[2].xyz)
#define scissorExt   frag[8].xy
#define scissorScale frag[8].zw
#define extent       frag[9].xy
#define radius       frag[9].z
#define feather      frag[9].w
#define strokeMult   frag[10].x
#define strokeThr    frag[10].y
#define texType      int(frag[10].z)
#define type         int(frag[10].w)

uniform vec4 frag[11];
uniform sampler2D tex;
varying vec2 ftcoord;
varying vec2 fpos;

float sdroundrect(vec2 pt, vec2 ext, float rad) {
  vec2 ext2 = ext - vec2(rad,rad);
  vec2 d = abs(pt) - ext2;
  return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;
}

// Scissoring
float scissorMask(vec2 p) {
  vec2 sc = (abs((scissorMat * vec3(p,1.0)).xy) - scissorExt);
  sc = vec2(0.5,0.5) - sc * scissorScale;
  return clamp(sc.x,0.0,1.0) * clamp(sc.y,0.0,1.0);
}
#ifdef EDGE_AA
// Stroke - from [0..1] to clipped pyramid, where the slope is 1px.
float strokeMask() {
  return min(1.0, (1.0-abs(ftcoord.x*2.0-1.0))*strokeMult) * min(1.0, ftcoord.y);
}
#endif

void main(void) {
   vec4 result;
  float scissor = scissorMask(fpos);
#ifdef EDGE_AA
  float strokeAlpha = strokeMask();
#else
  float strokeAlpha = 1.0;
#endif
  if (type == 0) {      // Gradient
    // Calculate gradient color using box gradient
    vec2 pt = (paintMat * vec3(fpos,1.0)).xy;
    float d = clamp((sdroundrect(pt, extent, radius) + feather*0.5) / feather, 0.0, 1.0);
    vec4 color = mix(innerCol,outerCol,d);
    // Combine alpha
    color *= strokeAlpha * scissor;
    result = color;
  } else if (type == 1) {    // Image
    // Calculate color from texture
    vec2 pt = (paintMat * vec3(fpos,1.0)).xy / extent;
    vec4 color = texture2D(tex, pt);
    if (texType == 1) color = vec4(color.xyz*color.w,color.w);
    if (texType == 2) color = vec4(color.x);
    // Apply color tint and alpha.
    color *= innerCol;
    // Combine alpha
    color *= strokeAlpha * scissor;
    result = color;
  } else if (type == 2) {    // Stencil fill
    result = vec4(1,1,1,1);
  } else if (type == 3) {    // Textured tris
    vec4 color = texture2D(tex, ftcoord);
    if (texType == 1) color = vec4(color.xyz*color.w,color.w);
    if (texType == 2) color = vec4(color.x);
    color *= scissor;
    result = color * innerCol;
  }
#ifdef EDGE_AA
  if (strokeAlpha < strokeThr) discard;
#endif
  gl_FragColor = result;
}
"

module Utils = struct
  let ibuf () =
    Bigarray.Array1.create
      Bigarray.int32 Bigarray.c_layout
      1

  let char_buf sz =
    let buf = Bigarray.(Array1.create Char c_layout sz) in
    buf, (fun sz -> String.init sz (Bigarray.Array1.get buf))

  let shader_info_log shader =
    let ibuf = ibuf () in
    let cbuf, to_str = char_buf 2048 in
    Gl.get_shader_info_log shader 2048 (Some ibuf) cbuf;
    to_str (Int32.to_int ibuf.{0})

  let create_shader ?version ?prefix source kind =
    let ibuf = ibuf () in
    let source = match prefix with
      | None -> source
      | Some prefix -> prefix ^ "\n" ^ source
    in
    let source = match version with
      | None -> source
      | Some v -> "#version " ^ string_of_int v ^ "\n" ^ source
    in
    let s = Gl.create_shader kind in
    Gl.shader_source s source;
    Gl.compile_shader s;
    Gl.get_shaderiv s Gl.compile_status ibuf;
    if Int32.to_int ibuf.{0} <> Gl.true_ then
      (Printf.eprintf "ERROR: GL shader index %d did not compile\n%s\n" s
         (shader_info_log s));
    s

  let program_info_log program =
    let ibuf = ibuf () in
    let cbuf, to_str = char_buf 2048 in
    Gl.get_program_info_log program 2048 (Some ibuf) cbuf;
    to_str (Int32.to_int ibuf.{0})

  let validate_program sp =
    let ibuf = ibuf () in
    Gl.validate_program sp;
    Gl.get_programiv sp Gl.validate_status ibuf;
    let result =Int32.to_int ibuf.{0} = Gl.true_ in
    if not result then
      prerr_endline (program_info_log sp);
    result

  let create_program ?version ?prefix bind vertex fragment =
    let ibuf = ibuf () in
    let vs = create_shader ?version ?prefix vertex Gl.vertex_shader
    and fs = create_shader ?version ?prefix fragment Gl.fragment_shader in
    let ps = Gl.create_program () in
    Gl.attach_shader ps vs;
    Gl.attach_shader ps fs;
    bind ps;
    Gl.link_program ps;
    Gl.get_programiv ps Gl.link_status ibuf;
    if Int32.to_int ibuf.{0} <> Gl.true_ then
      Printf.eprintf
        "ERROR: GL could not link shader program GL index %d\n%s\n"
        ps (program_info_log ps);
    assert (validate_program ps);
    Gl.delete_shader vs;
    Gl.delete_shader fs;
    ps

  let gen_buffer () =
    let ibuf = ibuf () in
    Gl.gen_buffers 1 ibuf;
    Int32.to_int ibuf.{0}

  let del_buffer vbo =
    let ibuf = ibuf () in
    ibuf.{0} <- Int32.of_int vbo;
    Gl.delete_buffers 1 ibuf
end

let create
    ~antialias
    ~stencil_strokes
    ~debug
  =
  let program =
    let prefix = if antialias then Some "#define EDGE_AA 1" else None in
    Utils.create_program ?prefix
      (fun program ->
         Gl.bind_attrib_location program 0 "vertex";
         Gl.bind_attrib_location program 1 "tcoord")
      shader_vertex shader_fragment
  in
  let viewsize  = Gl.get_uniform_location program "viewSize" in
  let viewxform = Gl.get_uniform_location program "viewXform" in
  let tex       = Gl.get_uniform_location program "tex" in
  let frag      = Gl.get_uniform_location program "frag" in
  let vert_vbo  = Utils.gen_buffer () in
  {
    program;
    viewsize; tex; frag; viewxform;
    vert_vbo;

    antialias; stencil_strokes; debug;

    font_glyphes = Hashtbl.create 8;
    font_todo = Hashtbl.create 8;
    font_buffer = None;
  }

let delete t =
  Gl.delete_program t.program;
  Utils.del_buffer t.vert_vbo

let fringe = 1.0

type obj =
  | Fill   of transform * Wall_tex.t paint * frame * T.bounds * V.path list
  | Stroke of transform * Wall_tex.t paint * frame * float * V.path list
  | Text   of transform * unit paint * frame * float * float * font * string

module Shader = struct

  let xfbuf =
    Bigarray.Array1.create
      Bigarray.float32 Bigarray.c_layout
      9

  let set_xform t xf =
    let open Transform in
    xfbuf.{00} <- xf.x00;
    xfbuf.{01} <- xf.x01;
    xfbuf.{02} <- 0.0;
    xfbuf.{03} <- xf.x10;
    xfbuf.{04} <- xf.x11;
    xfbuf.{05} <- 0.0;
    xfbuf.{06} <- xf.x20;
    xfbuf.{07} <- xf.x21;
    xfbuf.{08} <- 1.0;
    Gl.uniform3fv t.viewxform 3 xfbuf

  let buf =
    Bigarray.Array1.create
      Bigarray.float32 Bigarray.c_layout
      44

  let set_zero_m34 c =
    buf.{c + 00} <- 0.0;
    buf.{c + 01} <- 0.0;
    buf.{c + 02} <- 0.0;
    buf.{c + 03} <- 0.0;
    buf.{c + 04} <- 0.0;
    buf.{c + 05} <- 0.0;
    buf.{c + 06} <- 0.0;
    buf.{c + 07} <- 0.0;
    buf.{c + 08} <- 0.0;
    buf.{c + 09} <- 0.0;
    buf.{c + 10} <- 0.0;
    buf.{c + 11} <- 0.0

  let set_inv_xform c xf =
    let open Transform in
    let xf = inverse xf in
    buf.{c + 00} <- xf.x00;
    buf.{c + 01} <- xf.x01;
    buf.{c + 02} <- 0.0;
    buf.{c + 03} <- 0.0;
    buf.{c + 04} <- xf.x10;
    buf.{c + 05} <- xf.x11;
    buf.{c + 06} <- 0.0;
    buf.{c + 07} <- 0.0;
    buf.{c + 08} <- xf.x20;
    buf.{c + 09} <- xf.x21;
    buf.{c + 10} <- 1.0;
    buf.{c + 11} <- 0.0

  let set_4 c f0 f1 f2 f3 =
    buf.{c + 0} <- f0;
    buf.{c + 1} <- f1;
    buf.{c + 2} <- f2;
    buf.{c + 3} <- f3

  let set_color c a col =
    let r = Color.r col in
    let g = Color.g col in
    let b = Color.b col in
    let a = Color.a col *. a in
    set_4 c (r*.a) (g*.a) (b*.a) a

  let paint_mat   = 12
  let sciss_mat   = 0
  let inner_color = 24
  let outer_color = 28
  let sciss_extent_scale = 32
  let paint_extent_radius_feather = 36
  let strokemult_strokethr_textype_type = 40

  type shader_type = [
    | `FILLGRAD
    | `FILLIMG
    | `SIMPLE
    | `IMG
  ]

  let shader_type = function
    | `FILLGRAD -> 0.
    | `FILLIMG  -> 1.
    | `SIMPLE   -> 2.
    | `IMG      -> 3.

  let set_tool t ?typ paint frame width stroke_thr =
    let sextent = frame.Frame.extent in
    let sxform  = frame.Frame.xform in
    let alpha = frame.Frame.alpha in
    let alpha =
      if width < 1.0 then
        let da = min 1.0 (max 0.0 width (* /. fringe_width*)) in
        alpha *. da *. da
      else alpha
    in
    set_color inner_color alpha paint.Paint.inner;
    set_color outer_color alpha paint.Paint.outer;
    set_inv_xform paint_mat paint.Paint.xform;
    let sw = Size2.w sextent and sh = Size2.h sextent in
    if sw < -0.5 || sh < -0.5 then begin
      set_zero_m34 sciss_mat;
      set_4 sciss_extent_scale
        1.0 1.0 1.0 1.0
    end else begin
      let open Transform in
      set_inv_xform sciss_mat sxform;
      set_4 sciss_extent_scale sw sh
        (sqrt (sxform.x00 *. sxform.x00 +. sxform.x10 *. sxform.x10) /. fringe)
        (sqrt (sxform.x01 *. sxform.x01 +. sxform.x11 *. sxform.x11) /. fringe)
    end;
    let pw = Size2.w paint.Paint.extent and ph = Size2.h paint.Paint.extent in
    set_4 paint_extent_radius_feather pw ph
      paint.Paint.radius paint.Paint.feather;
    begin match paint.Paint.image with
      | None -> ()
      | Some tex -> Gl.bind_texture Gl.texture_2d (Wall_tex.tex tex);
    end;
    let typ = match typ, paint.Paint.image  with
      | None, Some _ -> `FILLIMG
      | None, None   -> `FILLGRAD
      | Some typ, _  -> typ
    in
    let texType = match paint.Paint.image with
      | Some image when Wall_tex.channels image >= 3 ->
        if Wall_tex.premultiplied image then 0.0 else 1.0
      | _ -> 2.0
    in
    set_4 strokemult_strokethr_textype_type
      ((width +. fringe) *. 0.5 /. fringe)
      stroke_thr
      texType (shader_type typ);
    Gl.uniform4fv t.frag 11 buf

  let set_simple t stroke_thr typ =
    for i = 0 to 43 do
      buf.{i} <- 0.0;
    done;
    buf.{strokemult_strokethr_textype_type + 1} <- stroke_thr;
    buf.{strokemult_strokethr_textype_type + 3} <- shader_type typ;
    Gl.uniform4fv t.frag 11 buf
end

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
  B.reserve vb (6 * 4);
  let convex = match paths with
    | [path] -> path.V.convex
    | _ -> false
  in
  let kind = if convex then CONVEXFILL else FILL in
  let {T. minx; miny; maxx; maxy} = bounds in
  let triangle_offset = B.offset vb / 4 in
  push_4 vb minx maxy 0.5 1.0;
  push_4 vb maxx maxy 0.5 1.0;
  push_4 vb maxx miny 0.5 1.0;
  push_4 vb minx maxy 0.5 1.0;
  push_4 vb maxx miny 0.5 1.0;
  push_4 vb minx miny 0.5 1.0;
  { kind; paths; width = 1.0; xform;
    triangle_offset; triangle_count = 6; paint; frame }

let exec_fill t
    { paint; frame; xform; width; paths; triangle_offset; triangle_count} =
  Shader.set_xform t xform;

  (* Draw shapes *)
  Gl.enable Gl.stencil_test;
  Gl.stencil_mask 0xff;
  Gl.stencil_func Gl.always 0 0xff;
  Gl.color_mask false false false false;

  (* set bindpoint for solid loc *)
  Shader.set_simple t (-1.0) `SIMPLE;
  Gl.stencil_op_separate Gl.front Gl.keep Gl.keep Gl.incr_wrap;
  Gl.stencil_op_separate Gl.back  Gl.keep Gl.keep Gl.decr_wrap;
  Gl.disable Gl.cull_face_enum;

  List.iter
    (fun path -> Gl.draw_arrays Gl.triangle_fan
        path.V.fill_first path.V.fill_count)
    paths;

  Gl.enable Gl.cull_face_enum;

  Gl.color_mask true true true true;
  Shader.set_tool t paint frame width (-1.0);

  (* Draw anti-aliased pixels *)
  if t.antialias then (
    Gl.stencil_func Gl.equal 0x00 0xff;
    Gl.stencil_op Gl.keep Gl.keep Gl.keep;
    List.iter
      (fun path -> Gl.draw_arrays Gl.triangle_strip
          path.V.stroke_first path.V.stroke_count)
      paths;
  );

  Gl.stencil_func Gl.notequal 0x0 0xff;
  Gl.stencil_op Gl.zero Gl.zero Gl.zero;
  Gl.draw_arrays Gl.triangles triangle_offset triangle_count;

  Gl.disable Gl.stencil_test

let exec_convex_fill t
    { paint; frame; xform; width; paths; triangle_offset; triangle_count} =
  Shader.set_xform t xform;
  Shader.set_tool t paint frame width (-1.0);
  List.iter
    (fun path -> Gl.draw_arrays Gl.triangle_fan
        path.V.fill_first path.V.fill_count)
    paths;
  if t.antialias then
    List.iter
      (fun path -> Gl.draw_arrays Gl.triangle_strip
          path.V.stroke_first path.V.stroke_count)
      paths

let prepare_stroke xform paint frame width paths =
  { kind = STROKE; xform; paint; frame; width; paths;
    triangle_offset = 0; triangle_count = 6 }

let exec_stroke t { frame; paint; width; paths; xform; } =
  if t.stencil_strokes then begin
    Gl.enable Gl.stencil_test;
    Gl.stencil_mask 0xff;

    (*  Fill the stroke base without overlap *)
    Gl.stencil_func Gl.equal 0x0 0xff;
    Gl.stencil_op Gl.keep Gl.keep Gl.incr;

    Shader.set_xform t xform;
    Shader.set_tool t paint frame width (1.0 -. 0.5 /. 255.0);

    List.iter
      (fun path -> Gl.draw_arrays Gl.triangle_strip
          path.V.stroke_first path.V.stroke_count)
      paths;

    (*  Draw anti-aliased pixels. *)
    Shader.set_tool t paint frame width (-1.0);

    Gl.stencil_func Gl.equal 0x00 0xff;
    Gl.stencil_op Gl.keep Gl.keep Gl.keep;

    List.iter
      (fun path -> Gl.draw_arrays Gl.triangle_strip
          path.V.stroke_first path.V.stroke_count)
      paths;

    (*  Clear stencil buffer. *)
    Gl.color_mask false false false false;
    Gl.stencil_func Gl.always 0x0 0xff;
    Gl.stencil_op Gl.zero Gl.zero Gl.zero;

    List.iter
      (fun path -> Gl.draw_arrays Gl.triangle_strip
          path.V.stroke_first path.V.stroke_count)
      paths;

    Gl.color_mask true true true true;

    Gl.disable Gl.stencil_test;

  end else begin

    Shader.set_xform t xform;
    Shader.set_tool t paint frame width (-1.0);

    (*  Draw Strokes *)
    List.iter
      (fun path -> Gl.draw_arrays Gl.triangle_strip
          path.V.stroke_first path.V.stroke_count)
      paths;

  end

let exec_triangles t { frame; xform; paint; triangle_offset; triangle_count } =
  Shader.set_xform t xform;
  Shader.set_tool t ~typ:`IMG paint frame 1.0 (-1.0);
  Gl.draw_arrays Gl.triangles  triangle_offset triangle_count

let frame_nr = ref 0

let alloc_text t = function
  | Text (xf, _, _, _, _, font, text) ->
    let _, key = Glyph.key xf font in
    let frame_nr = !frame_nr in
    let r = ref 0 in
    let len = String.length text in
    while !r < len do
      match utf8_decode r text with
      | -1 -> ()
      | cp ->
        let key = key cp in
        match Hashtbl.find t.font_glyphes key with
        | cache -> cache.Glyph.frame <- frame_nr
        | exception Not_found ->
          if not (Hashtbl.mem t.font_todo key) then
            Hashtbl.add t.font_todo key ()
    done
  | _ -> ()

let align_place factor x =
  let x = x +. factor *. 0.5 in x -. mod_float x factor

let place factor = function
  | `Exact -> (fun x -> x)
  | `Align -> align_place factor

let prepare_text t vb paint frame x y xform font text =
  let r = ref 0 in
  let len = String.length text in
  begin
    let count = ref 0 in
    while !r < len do if utf8_decode r text <> -1 then incr count done;
    B.reserve vb (!count * 4 * 6);
  end;
  r := 0;
  let offset = B.offset vb in
  let glyphes = font.Font.glyphes in
  let scale = Stb_truetype.scale_for_pixel_height glyphes font.Font.size in
  let factor, key = Glyph.key xform font in
  let xoff = ref 0 in
  let last = ref Stb_truetype.invalid_glyph in
  let place = place factor font.Font.placement in
  let y = place y in
  while !r < len do
    match utf8_decode r text with
    | -1 -> last := Stb_truetype.invalid_glyph
    | cp ->
      let key = key cp in
      match Hashtbl.find t.font_glyphes key with
      | { Glyph. box; uv; glyph; _ } ->
        let open Stb_truetype in
        xoff := !xoff + Stb_truetype.kern_advance glyphes !last glyph;
        last := glyph;
        (*Printf.eprintf
          "character { x0 = %d; y0 = %d; x1 = %d; y1 = %d }, factor %.02fx\n%!"
          box.x0 box.y0 box.x1 box.y1 factor;*)
        let x = place (x +. float !xoff *. scale) in
        let x0 = x +. float box.x0 *. factor in
        let y0 = y +. float box.y0 *. factor in
        let x1 = x +. float box.x1 *. factor in
        let y1 = y +. float box.y1 *. factor in
        let s0 = float uv.x0 /. 1024.0 in
        let t0 = float uv.y0 /. 1024.0 in
        let s1 = float uv.x1 /. 1024.0 in
        let t1 = float uv.y1 /. 1024.0 in
        push_4 vb x0 y0 s0 t0;
        push_4 vb x1 y1 s1 t1;
        push_4 vb x1 y0 s1 t0;
        push_4 vb x0 y0 s0 t0;
        push_4 vb x0 y1 s0 t1;
        push_4 vb x1 y1 s1 t1;
        xoff := !xoff + Stb_truetype.glyph_advance glyphes glyph;
      | exception Not_found ->
        last := Stb_truetype.invalid_glyph
  done;
  { kind = TRIANGLES; frame = Frame.default; paint; width = 1.0; paths = []; xform;
    triangle_offset = offset / 4;
    triangle_count  = (B.offset vb - offset) / 4;
  }

let prepare_obj t vbuffer = function
  | Fill (xform, paint, frame, bounds, paths) ->
    prepare_fill vbuffer xform paint frame bounds paths
  | Stroke (xform, paint, frame, width, paths) ->
    prepare_stroke xform paint frame width paths
  | Text (xform, paint, frame, x, y, font, text) ->
    begin match t.font_buffer with
      | None -> { kind = TRIANGLES; frame = Frame.default; paint = Paint.black;
                  width = 1.0; paths = []; xform = Transform.identity;
                  triangle_offset = 0; triangle_count = 0; }
      | Some buffer ->
        let paint = {paint with Paint.image = Some buffer.texture} in
        prepare_text t vbuffer paint frame x y xform font text
    end

let exec_call t call =
  match call.kind with
  | FILL -> exec_fill t call
  | CONVEXFILL -> exec_convex_fill t call
  | STROKE -> exec_stroke t call
  | TRIANGLES -> exec_triangles t call

let new_font_buffer width height =
  let data = Bigarray.(Array1.create int8_unsigned c_layout (width * height)) in
  let image = {Stb_image. width = width; height = height; channels = 1; data } in
  let texture = Wall_tex.from_image ~name:"font atlas" image in
  let room = Maxrects.add_bin () width height Maxrects.empty in
  { image; texture; room }

let box_offset {Stb_truetype. x0; x1; y0; y1 } p =
  {Stb_truetype. x0 = x0 - p; x1 = x1 + p; y0 = y0 - p; y1 = y1 + p }

let bake_glyphs t =
  let buffer = match t.font_buffer with
    | Some buffer -> buffer
    | None ->
      let buffer = new_font_buffer 1024 1024 in
      t.font_buffer <- Some buffer;
      buffer
  in
  let add_box ({ Glyph. scale; cp; ttf; blur } as key) () boxes =
    match Stb_truetype.find ttf cp with
    | None -> boxes
    | Some glyph ->
      let scale = Stb_truetype.scale_for_pixel_height ttf (float scale /. 10.0) in
      let box = Stb_truetype.get_glyph_bitmap_box ttf glyph ~scale_x:scale ~scale_y:scale in
      let {Stb_truetype. x0; y0; x1; y1} = box in
      Maxrects.box (key, ttf, glyph, scale, box)
        (x1 - x0 + 2 + blur / 10) (y1 - y0 + 2 + blur / 10) :: boxes
  in
  let boxes = Hashtbl.fold add_box t.font_todo [] in
  let room, boxes = Maxrects.insert_batch buffer.room boxes in
  buffer.room <- room;
  List.iter (function
      | None -> ()
      | Some {Maxrects. x; y; w; h; box; bin =_} ->
        let (key, ttf, glyph, scale, box) = box.Maxrects.tag in
        let pad = 1 + key.Glyph.blur / 20 in
        let uv = {Stb_truetype. x0 = x + pad; x1 = x + w - pad;
                  y0 = y + pad; y1 = y + h - pad} in
        Stb_truetype.make_glyph_bitmap
          ttf
          buffer.image.Stb_image.data
          ~width:buffer.image.Stb_image.width
          ~height:buffer.image.Stb_image.height
          ~scale_x:scale
          ~scale_y:scale
          uv
          glyph;
        let uv, box = if key.Glyph.blur = 0 then uv, box else (
            let uv = box_offset uv pad and box = box_offset box pad in
            Stb_truetype.blur_glyph_bitmap
              buffer.image.Stb_image.data
              ~width:buffer.image.Stb_image.width
              ~height:buffer.image.Stb_image.height
              uv
              (float key.Glyph.blur /. 10.0);
            uv, box
          )
        in
        Hashtbl.add t.font_glyphes key { Glyph. box; uv; frame = !frame_nr; glyph }
    ) boxes;
  Hashtbl.reset t.font_todo;
  Wall_tex.update buffer.texture buffer.image

let render t viewsize vbuffer objs =
  (* Allocate font glyphs *)
  incr frame_nr;
  List.iter (alloc_text t) objs;
  if Hashtbl.length t.font_todo <> 0 then (
    bake_glyphs t
  );
  let calls = List.map (prepare_obj t vbuffer) objs in

  Gl.use_program    t.program;
  Gl.blend_func     Gl.one Gl.one_minus_src_alpha;
  Gl.enable         Gl.cull_face_enum;
  Gl.cull_face      Gl.back;
  Gl.front_face     Gl.ccw;
  Gl.enable         Gl.blend;
  Gl.disable        Gl.depth_test;
  Gl.disable        Gl.scissor_test;
  Gl.color_mask     true true true true;
  Gl.stencil_mask   0xffffffff;
  Gl.stencil_op     Gl.keep Gl.keep Gl.keep;
  Gl.stencil_func   Gl.always 0 0xffffffff;
  Gl.active_texture Gl.texture0;
  Gl.bind_texture   Gl.texture_2d 0;

  (*  Upload vertex data *)
  Gl.bind_buffer Gl.array_buffer t.vert_vbo;
  let sub = B.sub vbuffer in
  Gl.buffer_data Gl.array_buffer
    (Bigarray.Array1.dim sub * 4) (Some sub) Gl.stream_draw;

  Gl.enable_vertex_attrib_array 0;
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_pointer 0 2 Gl.float false (4 * 4) (`Offset 0);
  Gl.vertex_attrib_pointer 1 2 Gl.float false (4 * 4) (`Offset (2 * 4));

  (*  Set view and texture just once per frame. *)
  Gl.uniform1i t.tex 0;
  Gl.uniform2f t.viewsize (Size2.w viewsize) (Size2.h viewsize);

  List.iter (exec_call t) calls;

  Gl.disable_vertex_attrib_array 0;
  Gl.disable_vertex_attrib_array 1;
  Gl.disable Gl.cull_face_enum;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.use_program 0;
  Gl.bind_texture Gl.texture_2d 0
