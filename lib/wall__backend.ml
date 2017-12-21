open Tgles2
open Wall
open Gg

type t = {
  program   : int;
  viewsize  : int;
  viewxform : int;
  tex       : int;
  frag      : int;
  vert_vbo  : int;
}

(* OpenGL renderer from
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
  if (strokeAlpha < strokeThr) discard;
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
  } else if (type == 3) {    // Texture atlas
    vec4 color = texture2D(tex, ftcoord, -0.66);
    if (texType == 1) color = vec4(color.xyz*color.w,color.w);
    if (texType == 2) color = vec4(color.x);
    color *= scissor;
    result = color * innerCol;
  }
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

let create ~antialias =
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
  }

let delete t =
  Gl.delete_program t.program;
  Utils.del_buffer t.vert_vbo

let fringe = 1.0

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

  let clampf min x max : float =
    if x < min then x else if x > max then max else x

  let set_tool t ?typ paint frame width stroke_thr =
    let sextent = frame.Frame.extent in
    let sxform  = frame.Frame.xform in
    let alpha = frame.Frame.alpha in
    let alpha =
      if width < 1.0 then
        let da = clampf 0.0 (width (*/. fringe_width*)) 1.0 in
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

module Fill = struct
  let prepare_stencil t xform =
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
    Gl.disable Gl.cull_face_enum

  let draw_stencil first count =
    Gl.draw_arrays Gl.triangle_fan first count

  let prepare_cover t paint frame width =
    Gl.enable Gl.cull_face_enum;
    Gl.color_mask true true true true;
    Shader.set_tool t paint frame width (-1.0)

  let prepare_aa () =
    Gl.stencil_func Gl.equal 0x00 0xff;
    Gl.stencil_op Gl.keep Gl.keep Gl.keep

  let draw_aa first count =
    Gl.draw_arrays Gl.triangle_strip first count

  let finish_and_cover first count =
    Gl.stencil_func Gl.notequal 0x0 0xff;
    Gl.stencil_op Gl.zero Gl.zero Gl.zero;
    Gl.draw_arrays Gl.triangle_strip first count;
    Gl.disable Gl.stencil_test
end

module Convex_fill = struct
  let prepare t xform paint frame width =
    Shader.set_xform t xform;
    Shader.set_tool t paint frame width (-1.0)

  let draw first count =
    Gl.draw_arrays Gl.triangle_fan first count

  let draw_aa first count =
    Gl.draw_arrays Gl.triangle_strip first count
end

module Stencil_stroke = struct
  let prepare_stencil t xform paint frame width =
    Gl.enable Gl.stencil_test;
    Gl.stencil_mask 0xff;

    (*  Fill the stroke base without overlap *)
    Gl.stencil_func Gl.equal 0x0 0xff;
    Gl.stencil_op Gl.keep Gl.keep Gl.incr;

    Shader.set_xform t xform;
    Shader.set_tool t paint frame width (1.0 -. 0.5 /. 255.0)

  let draw_stencil first count =
    Gl.draw_arrays Gl.triangle_strip first count

  let prepare_aa t paint frame width =
    Shader.set_tool t paint frame width (-1.0);
    Gl.stencil_func Gl.equal 0x00 0xff;
    Gl.stencil_op Gl.keep Gl.keep Gl.keep

  let draw_aa first count =
    Gl.draw_arrays Gl.triangle_strip first count

  let prepare_clear () =
    Gl.color_mask false false false false;
    Gl.stencil_func Gl.always 0x0 0xff;
    Gl.stencil_op Gl.zero Gl.zero Gl.zero

  let draw_clear first count =
    Gl.draw_arrays Gl.triangle_strip first count

  let finish () =
    Gl.color_mask true true true true;
    Gl.disable Gl.stencil_test
end

module Direct_stroke = struct
  let prepare t xform paint frame width =
    Shader.set_xform t xform;
    Shader.set_tool t paint frame width (-1.0)

  let draw first count =
    Gl.draw_arrays Gl.triangle_strip first count
end

module Triangles = struct
  let prepare t xform paint frame =
    Shader.set_xform t xform;
    Shader.set_tool t ~typ:`IMG paint frame 1.0 (-1.0)

  let draw first count =
    Gl.draw_arrays Gl.triangles first count
end

let gl_reversed = ref false

let force_set_reversed flag =
  Gl.front_face (if flag then Gl.cw else Gl.ccw);
  gl_reversed := flag

let set_reversed xform =
  let reversing_transform {Transform. x00; x10; x01; x11; _} =
    x00 *. x11 < x01 *. x10
  in
  let reversing = reversing_transform xform in
  if reversing <> !gl_reversed then
    force_set_reversed reversing

let prepare t width height (data : Wall__geom.B.bigarray) =
  (* Setup gl state *)
  Gl.use_program    t.program;
  Gl.blend_func     Gl.one Gl.one_minus_src_alpha;
  Gl.enable         Gl.cull_face_enum;
  Gl.cull_face      Gl.back;
  gl_reversed       := false;
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
  (* Upload vertex data *)
  Gl.bind_buffer Gl.array_buffer t.vert_vbo;
  Gl.buffer_data Gl.array_buffer
    (Bigarray.Array1.dim data * 4) (Some data) Gl.stream_draw;
  Gl.enable_vertex_attrib_array 0;
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_pointer 0 2 Gl.float false (4 * 4) (`Offset 0);
  Gl.vertex_attrib_pointer 1 2 Gl.float false (4 * 4) (`Offset (2 * 4));
  (* Set view and texture just once per frame. *)
  Gl.uniform1i t.tex 0;
  Gl.uniform2f t.viewsize width height

let finish () =
  Gl.disable_vertex_attrib_array 0;
  Gl.disable_vertex_attrib_array 1;
  Gl.disable Gl.cull_face_enum;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.use_program 0;
  Gl.bind_texture Gl.texture_2d 0
