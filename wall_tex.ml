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

type t = {
  name: string;
  mutable tex: int;
  width: int;
  height: int;
}

let release t =
  if t.tex <> -1 then begin
    let buf = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
    buf.{0} <- Int32.of_int t.tex;
    t.tex <- -1;
    Gl.delete_textures 1 buf
  end

let finalize t =
  if t.tex <> -1 then begin
    prerr_endline
      ("Wall_tex warning: texture " ^ t.name ^ " has not been released");
    release t
  end


let make ~name width height =
  let buf = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  Gl.gen_textures 1 buf;
  let result = {
    name; width; height;
    tex = Int32.to_int buf.{0};
  } in
  Gc.finalise finalize result;
  result

let tex t =
  if t.tex <> -1 then t.tex
  else invalid_arg ("Wall_tex.tex: " ^ t.name ^ " has been released")

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

let channels image =
  match image.Stb_image.channels with
  | 3 -> Gl.rgb
  | 4 -> Gl.rgba
  | _ -> invalid_arg "Wall_tex: unsupported image format"

let format (type a) (type b)
    (image : (a, b) Bigarray.kind Stb_image.t) =
  match Bigarray.Array1.kind image.Stb_image.data with
  | Bigarray.Int8_unsigned -> Gl.unsigned_byte
  | Bigarray.Float32 -> Gl.float
  | _ -> invalid_arg "Wall_tex: unsupported image format"

let from_image ~name image =
  let t = make ~name image.Stb_image.width image.Stb_image.height in
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d (tex t);
  Gl.pixel_storei Gl.unpack_alignment 1;
  Gl.tex_image2d
    Gl.texture_2d
    0
    (channels image)
    image.Stb_image.width
    image.Stb_image.height
    0
    (channels image)
    (format image)
    (`Data image.Stb_image.data);
  Gl.pixel_storei Gl.unpack_alignment 4;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear_mipmap_linear;
  (* TODO: setup anisotropic filter *)
  Gl.generate_mipmap Gl.texture_2d;
  Gl.bind_texture Gl.texture_2d 0;
  t

let load_image ?(float=false) ?(alpha=true) ?(flip=false) ?name s =
  let channels = if alpha then 4 else 3 in
  let name = match name with
    | None -> s
    | Some name -> name
  in
  let load = function
    | `Error msg ->
      Result.Error (`Msg msg)
    | `Ok image ->
      if flip then flip_image image;
      let t = from_image ~name image in
      Stb_image.free_unmanaged image;
      Result.Ok t
  in
  if float then
    load (Stb_image.loadf_unmanaged ~channels s)
  else
    load (Stb_image.load_unmanaged ~channels s)

let width t = t.width
let height t = t.height
