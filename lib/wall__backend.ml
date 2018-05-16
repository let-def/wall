open Wall_types
open Gg
open Bigarray

type t

external wall_gl_create
  : antialias:bool -> t
  = "wall_gl_create"

external wall_gl_delete
  : t -> unit
  = "wall_gl_delete"

external wall_gl_is_valid
  : t -> bool
  = "wall_gl_is_valid" [@@noalloc]

external wall_gl_bind_xform
  : t -> Wall__geom.B.bigarray -> unit
  = "wall_gl_bind_xform" [@@noalloc]

external wall_gl_bind_paint
  : t -> Wall__geom.B.bigarray -> unit
  = "wall_gl_bind_paint" [@@noalloc]

external wall_gl_bind_texture
  : int -> unit
  = "wall_gl_bind_texture" [@@noalloc]

external wall_gl_draw_triangle_fan
  : first:int -> count:int -> unit
  = "wall_gl_draw_triangle_fan" [@@noalloc]

external wall_gl_draw_triangle_strip
  : first:int -> count:int -> unit
  = "wall_gl_draw_triangle_strip" [@@noalloc]

external wall_gl_draw_triangles
  : first:int -> count:int -> unit
  = "wall_gl_draw_triangles" [@@noalloc]

external wall_gl_fill_prepare_stencil
  : unit -> unit
  = "wall_gl_fill_prepare_stencil" [@@noalloc]

external wall_gl_fill_prepare_cover
  : unit -> unit
  = "wall_gl_fill_prepare_cover" [@@noalloc]

external wall_gl_prepare_aa
  : unit -> unit
  = "wall_gl_prepare_aa" [@@noalloc]

external wall_gl_fill_finish_and_cover
  : first:int -> count:int -> unit
  = "wall_gl_fill_finish_and_cover" [@@noalloc]

external wall_gl_stencil_stroke_prepare_stencil
  : unit -> unit
  = "wall_gl_stencil_stroke_prepare_stencil" [@@noalloc]

external wall_gl_stencil_stroke_prepare_clear
  : unit -> unit
  = "wall_gl_stencil_stroke_prepare_clear" [@@noalloc]

external wall_gl_stencil_stroke_finish
  : unit -> unit
  = "wall_gl_stencil_stroke_finish" [@@noalloc]

external wall_gl_set_reversed
  : bool -> unit
  = "wall_gl_set_reversed" [@@noalloc]

external wall_gl_frame_prepare
  : t -> width:float -> height:float -> Wall__geom.B.bigarray -> unit
  = "wall_gl_frame_prepare"

external wall_gl_frame_finish
  : unit -> unit
  = "wall_gl_frame_finish"

external wall_gl_texture_create
  : unit -> int
  = "wall_gl_texture_create"

external wall_gl_texture_delete
  : int -> unit
  = "wall_gl_texture_delete"

external wall_gl_texture_upload
  : int -> level:int -> is_float:bool ->
    width:int -> height:int -> channels:int ->
    ('a, 'b, c_layout) Array1.t -> offset:int -> stride:int -> unit
  = "wall_gl_texture_upload_bc" "wall_gl_texture_upload"

external wall_gl_texture_update
  : int -> level:int -> is_float:bool ->
    x:int -> y:int -> width:int -> height:int -> channels:int ->
    ('a, 'b, c_layout) Array1.t -> offset:int -> stride:int -> unit
  = "wall_gl_texture_update_bc" "wall_gl_texture_update"

external wall_gl_texture_generate_mipmap
  : int -> unit
  = "wall_gl_texture_generate_mipmap"

let create = wall_gl_create

let delete = wall_gl_delete

let fringe = 1.0

module Texture = struct
  type t = int

  type specification = {
    gl_tex : int;
    premultiplied : bool;
    channels : int;
  }

  let create = wall_gl_texture_create
  let delete = wall_gl_texture_delete

  let is_float (type a) (type b) (image : (a, b) Bigarray.kind Stb_image.t) =
    match Bigarray.Array1.kind image.Stb_image.data with
    | Bigarray.Int8_unsigned -> false
    | Bigarray.Float32 -> true
    | _ -> invalid_arg "wall: unsupported image format"

  let channels img =
    match Stb_image.channels img with
    | 1 | 3 | 4 as c -> c
    | c ->
      failwith ("wall: " ^ string_of_int c ^ " channels texture format not supported")

  let upload ?(level=0) img t =
    wall_gl_texture_upload t ~level ~is_float:(is_float img)
      ~width:(Stb_image.width img) ~height:(Stb_image.height img)
      ~channels:(channels img)
      (Stb_image.data img)
      ~offset:img.Stb_image.offset ~stride:img.Stb_image.stride

  let update ?(level=0) ~x ~y img t =
    wall_gl_texture_update t ~level ~is_float:(is_float img)
      ~x ~y ~width:(Stb_image.width img) ~height:(Stb_image.height img)
      ~channels:(channels img)
      (Stb_image.data img)
      ~offset:img.Stb_image.offset ~stride:img.Stb_image.stride

  let generate_mipmap = wall_gl_texture_generate_mipmap
end

module Shader = struct

  let xfbuf =
    Bigarray.Array1.create
      Bigarray.float32 Bigarray.c_layout
      9

  let set_xform t xf =
    xfbuf.{00} <- xf.x00;
    xfbuf.{01} <- xf.x01;
    xfbuf.{02} <- 0.0;
    xfbuf.{03} <- xf.x10;
    xfbuf.{04} <- xf.x11;
    xfbuf.{05} <- 0.0;
    xfbuf.{06} <- xf.x20;
    xfbuf.{07} <- xf.x21;
    xfbuf.{08} <- 1.0;
    wall_gl_bind_xform t xfbuf

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

  let set_inv_xform c xf invdet =
    let x00 =    xf.x11 *. invdet in
    let x10 = -. xf.x10 *. invdet in
    let x20 =    (xf.x10 *. xf.x21 -. xf.x11 *. xf.x20) *. invdet in
    let x01 = -. xf.x01 *. invdet in
    let x11 =    xf.x00 *. invdet in
    let x21 =    (xf.x01 *. xf.x20 -. xf.x00 *. xf.x21) *. invdet in
    buf.{c + 00} <- x00;
    buf.{c + 01} <- x01;
    buf.{c + 02} <- 0.0;
    buf.{c + 03} <- 0.0;
    buf.{c + 04} <- x10;
    buf.{c + 05} <- x11;
    buf.{c + 06} <- 0.0;
    buf.{c + 07} <- 0.0;
    buf.{c + 08} <- x20;
    buf.{c + 09} <- x21;
    buf.{c + 10} <- 1.0;
    buf.{c + 11} <- 0.0

  let set_inv_xform c xf =
    let det = xf.x00 *. xf.x11 -. xf.x10 *. xf.x01 in
    if det > -1e-6 && det < 1e-6 then (
      buf.{c + 00} <- 1.0;
      buf.{c + 01} <- 0.0;
      buf.{c + 02} <- 0.0;
      buf.{c + 03} <- 0.0;
      buf.{c + 04} <- 0.0;
      buf.{c + 05} <- 1.0;
      buf.{c + 06} <- 0.0;
      buf.{c + 07} <- 0.0;
      buf.{c + 08} <- 0.0;
      buf.{c + 09} <- 0.0;
      buf.{c + 10} <- 1.0;
      buf.{c + 11} <- 0.0
    ) else
      set_inv_xform c xf (1.0 /. det)

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

  let set_tool t ?typ prj paint frame width stroke_thr =
    let sextent = frame.extent in
    let sxform  = frame.xform in
    let alpha = frame.alpha in
    let alpha =
      (*if width < 1.0 then
        let da = clampf 0.0 (width (*/. fringe_width*)) 1.0 in
        alpha *. da *. da
      else*) alpha
    in
    set_color inner_color alpha paint.inner;
    set_color outer_color alpha paint.outer;
    set_inv_xform paint_mat paint.xform;
    let sw = Size2.w sextent and sh = Size2.h sextent in
    if sw < -0.5 || sh < -0.5 then begin
      set_zero_m34 sciss_mat;
      set_4 sciss_extent_scale
        1.0 1.0 1.0 1.0
    end else begin
      set_inv_xform sciss_mat sxform;
      set_4 sciss_extent_scale sw sh
        (sqrt (sxform.x00 *. sxform.x00 +. sxform.x10 *. sxform.x10) /. fringe)
        (sqrt (sxform.x01 *. sxform.x01 +. sxform.x11 *. sxform.x11) /. fringe)
    end;
    let pw = Size2.w paint.extent and ph = Size2.h paint.extent in
    set_4 paint_extent_radius_feather pw ph paint.radius paint.feather;
    let typ = match typ, paint.texture  with
      | None, Some _ -> `FILLIMG
      | None, None   -> `FILLGRAD
      | Some typ, _  -> typ
    in
    let texType = match paint.texture with
      | None -> 2.0
      | Some tex ->
        let {Texture. premultiplied; channels; gl_tex} = prj tex in
        wall_gl_bind_texture gl_tex;
        if channels >= 3 then
          if premultiplied then 0.0 else 1.0
        else
          2.0
    in
    set_4 strokemult_strokethr_textype_type
      (if stroke_thr = -2.0 then 0.0 else width) (*((width +. fringe) *. 0.5 /. fringe)*)
      stroke_thr
      texType (shader_type typ);
    wall_gl_bind_paint t buf

  let set_simple t stroke_thr typ =
    for i = 0 to 43 do
      buf.{i} <- 0.0;
    done;
    buf.{strokemult_strokethr_textype_type + 0} <- 0.0;
    buf.{strokemult_strokethr_textype_type + 1} <- stroke_thr;
    buf.{strokemult_strokethr_textype_type + 3} <- shader_type typ;
    wall_gl_bind_paint t buf
end

module Fill = struct
  let prepare_stencil t =
    (* Draw shapes *)
    wall_gl_fill_prepare_stencil ();
    (* set bindpoint for solid loc *)
    Shader.set_simple t (-1.0) `SIMPLE

  let draw_stencil = wall_gl_draw_triangle_fan

  let prepare_cover t prj paint frame =
    wall_gl_fill_prepare_cover ();
    Shader.set_tool t prj paint frame 1.0 (-2.0)

  let prepare_aa = wall_gl_prepare_aa

  let draw_aa = wall_gl_draw_triangle_strip

  let finish_and_cover = wall_gl_fill_finish_and_cover
end

module Convex_fill = struct
  let prepare t prj paint frame =
    Shader.set_tool t prj paint frame 1.0 (-2.0)

  let draw = wall_gl_draw_triangle_fan

  let draw_aa = wall_gl_draw_triangle_strip
end

module Stencil_stroke = struct
  let prepare_stencil t prj paint frame width =
    (*  Fill the stroke base without overlap *)
    wall_gl_stencil_stroke_prepare_stencil ();
    Shader.set_tool t prj paint frame width (1.0 -. 0.5 /. 255.0)

  let draw_stencil = wall_gl_draw_triangle_strip

  let prepare_aa t prj paint frame width =
    Shader.set_tool t prj paint frame width (-1.0);
    wall_gl_prepare_aa ()

  let draw_aa = wall_gl_draw_triangle_strip

  let prepare_clear = wall_gl_stencil_stroke_prepare_clear

  let draw_clear = wall_gl_draw_triangle_strip

  let finish = wall_gl_stencil_stroke_finish
end

module Direct_stroke = struct
  let prepare t prj paint frame width =
    Shader.set_tool t prj paint frame width (-1.0)

  let draw = wall_gl_draw_triangle_strip
end

module Triangles = struct
  let prepare t prj paint frame =
    Shader.set_tool t ~typ:`IMG prj paint frame 1.0 (-2.0)

  let draw = wall_gl_draw_triangles
end

let gl_reversed = ref false

let force_set_reversed flag =
  wall_gl_set_reversed flag;
  gl_reversed := flag

let set_reversed xf =
  let reversing = xf.x00 *. xf.x11 < xf.x01 *. xf.x10 in
  if reversing <> !gl_reversed then
    force_set_reversed reversing

let set_xform t xf =
  Shader.set_xform t xf;
  set_reversed xf

let prepare t ~width ~height data =
  gl_reversed       := false;
  (* Setup gl state *)
  wall_gl_frame_prepare t ~width ~height data

let finish = wall_gl_frame_finish

external memory_spent : unit -> int = "wall_memory_spent" [@@noalloc]
external time_spent : unit -> int = "wall_time_spent" [@@noalloc]
