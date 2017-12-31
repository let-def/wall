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

module Backend = Wall__backend

let invalid =
  {Backend.Texture. gl_tex = -1; channels = 0; premultiplied = false}

type t = {
  name: string;
  mutable tex: Backend.Texture.specification;
  mutable width: int;
  mutable height: int;
}

let release t =
  if t.tex.gl_tex <> -1 then begin
    Backend.Texture.delete t.tex.gl_tex;
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
  Backend.Texture.upload ~level:0 image t.tex.gl_tex;
  Backend.Texture.generate_mipmap t.tex.gl_tex;
  t.tex <- {t.tex with Backend.Texture.channels = image.Stb_image.channels};
  t.width <- image.Stb_image.width;
  t.height <- image.Stb_image.height

let sub_update t ~x ~y image =
  validate t;
  Backend.Texture.update ~level:0 ~x ~y image t.tex.gl_tex;
  Backend.Texture.generate_mipmap t.tex.gl_tex

let from_image ~name image =
  let tex = {
    Backend.Texture.
    gl_tex = Backend.Texture.create ();
    premultiplied = true;
    channels = 0
  } in
  let t = { name; width = 0; height = 0; tex } in
  Gc.finalise finalize t;
  update t image;
  t

let load_image ?(float=false) ?(alpha=true) ?(flip=false) ?name s =
  let channels = if alpha then 4 else 3 in
  let name = match name with
    | None -> s
    | Some name -> name
  in
  let load = function
    | Result.Error _ as error -> error
    | Result.Ok image ->
      if flip then flip_image image;
      let t = from_image ~name image in
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
