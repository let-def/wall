open Wall
open Wall__geom

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

type font_buffer = {
  image: Stb_image.int8 Stb_image.t;
  texture: Wall_tex.t;
  mutable room : unit Maxrects.t;
}

type font_stash = {
  font_glyphes: (Glyph.key, Glyph.cell) Hashtbl.t;
  font_todo: (Glyph.key, unit) Hashtbl.t;
  mutable font_buffer: font_buffer option;
}

let font_stash () = {
  font_glyphes = Hashtbl.create 8;
  font_todo = Hashtbl.create 8;
  font_buffer = None;
}

let align_place factor x =
  let x = x +. factor *. 0.5 in x -. mod_float x factor

let place factor = function
  | `Exact -> (fun x -> x)
  | `Align -> align_place factor

let render_glyphes stash xform (font,pos,text) (push : _ -> unit) =
  let x = Gg.P2.x pos and y = Gg.P2.y pos in
  let glyphes = font.Font.glyphes in
  let scale = Stb_truetype.scale_for_pixel_height glyphes font.Font.size in
  let factor, key = Glyph.key xform font in
  let xoff = ref 0 in
  let last = ref Stb_truetype.invalid_glyph in
  let place = place factor font.Font.placement in
  let y = place y in
  let r = ref 0 in
  let len = String.length text in
  while !r < len do
    match utf8_decode r text with
    | -1 -> last := Stb_truetype.invalid_glyph
    | cp ->
      let key = key cp in
      match Hashtbl.find stash.font_glyphes key with
      | { Glyph. box; uv; glyph; _ } ->
        let open Stb_truetype in
        xoff := !xoff + Stb_truetype.kern_advance glyphes !last glyph;
        last := glyph;
        (*Printf.eprintf
          "character { x0 = %d; y0 = %d; x1 = %d; y1 = %d }, factor %.02fx\n%!"
          box.x0 box.y0 box.x1 box.y1 factor;*)
        let x = place (x +. float !xoff *. scale) in
        let bx0 = x +. float box.x0 *. factor in
        let by0 = y +. float box.y0 *. factor in
        let bx1 = x +. float box.x1 *. factor in
        let by1 = y +. float box.y1 *. factor in
        let s0 = float uv.x0 /. 1024.0 in
        let t0 = float uv.y0 /. 1024.0 in
        let s1 = float uv.x1 /. 1024.0 in
        let t1 = float uv.y1 /. 1024.0 in
        push {Stb_truetype. bx0; by0; bx1; by1; s0; t0; s1; t1 };
        xoff := !xoff + Stb_truetype.glyph_advance glyphes glyph;
      | exception Not_found ->
        last := Stb_truetype.invalid_glyph
  done;
  match stash.font_buffer with
  | None -> failwith "wall_glyph: not font buffer"
  | Some buf -> buf.texture

let ok = function
  | Result.Ok x -> x
  | Result.Error (`Msg msg) -> failwith msg

let new_font_buffer width height =
  let data = Bigarray.(Array1.create int8_unsigned c_layout (width * height)) in
  let image = ok (Stb_image.image ~width ~height ~channels:1 data) in
  let texture = Wall_tex.from_image ~name:"font atlas" image in
  let room = Maxrects.add_bin () width height Maxrects.empty in
  { image; texture; room }

let box_offset {Stb_truetype. x0; x1; y0; y1 } p =
  {Stb_truetype. x0 = x0 - p; x1 = x1 + p; y0 = y0 - p; y1 = y1 + p }

let frame_nr = ref 0

let allocate_glyphes stash xf (font,_pos,text) =
  let _, key = Glyph.key xf font in
  let r = ref 0 in
  let len = String.length text in
  let frame_nr = !frame_nr in
  while !r < len do
    match utf8_decode r text with
    | -1 -> ()
    | cp ->
      let key = key cp in
      match Hashtbl.find stash.font_glyphes key with
      | cache -> cache.Glyph.frame <- frame_nr
      | exception Not_found ->
        if not (Hashtbl.mem stash.font_todo key) then
          Hashtbl.add stash.font_todo key ()
  done

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
  let todo = Hashtbl.fold add_box t.font_todo [] in
  let room, boxes = Maxrects.insert_batch buffer.room todo in
  let room, boxes =
    if List.exists (function None -> true | _ -> false) boxes then (
      let todo = Hashtbl.fold
          (fun key cell todo ->
             if cell.Glyph.frame = !frame_nr then add_box key () todo else todo)
          t.font_glyphes todo
      in
      Hashtbl.reset t.font_glyphes;
      Bigarray.Array1.fill (Stb_image.data buffer.image) 0;
      let room = Maxrects.add_bin ()
          (Stb_image.width buffer.image)
          (Stb_image.height buffer.image)
          Maxrects.empty
      in
      Maxrects.insert_batch room todo
    ) else (room, boxes)
  in
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
  Wall_tex.update buffer.texture buffer.image;
  incr frame_nr

let typesetter () =
  let stash = font_stash () in
  Wall.Typesetter.make
    ~allocate:(allocate_glyphes stash)
    ~bake:(fun _ _ ->
        if Hashtbl.length stash.font_todo > 0 then bake_glyphs stash)
    ~render:(render_glyphes stash)
