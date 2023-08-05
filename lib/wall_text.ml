open Wall
open Wall__geom

(* utf-8 decoding dfa, from http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ *)

let bufsize = 2048
let ibufsize = 1.0 /. float bufsize

let utf8d =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\
   \007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
   \b\b\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\
   \n\003\003\003\003\003\003\003\003\003\003\003\003\004\003\003\
   \011\006\006\006\005\b\b\b\b\b\b\b\b\b\b\b\
   \000\001\002\003\005\b\007\001\001\001\004\006\001\001\001\001\
   \001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\000\001\001\001\001\001\000\001\000\001\001\001\001\001\001\
   \001\002\001\001\001\001\001\002\001\002\001\001\001\001\001\001\001\001\001\001\001\001\001\002\001\001\001\001\001\001\001\001\
   \001\002\001\001\001\001\001\001\001\002\001\001\001\001\001\001\001\001\001\001\001\001\001\003\001\003\001\001\001\001\001\001\
   \001\003\001\001\001\001\001\003\001\003\001\001\001\001\001\001\001\003\001\001\001\001\001\001\001\001\001\001\001\001\001\001"

let utf8_decode index str =
  let codep = ref 0 in
  let state = ref 0 in
  let len = String.length str in
  let index' = ref !index in
  while (
    !index' < len &&
    let c = Char.code (String.get str !index') in
    let t = Char.code (String.unsafe_get utf8d c) in
    codep := (if !state <> 0 then (c land 0x3f) lor (!codep lsl 6) else (0xff lsr t) land c);
    state := Char.code (String.unsafe_get utf8d (256 + !state * 16 + t) );
    incr index';
    !state > 1
  ) do ()
  done;
  index := !index';
  if !state = 0 then !codep else (-1)

module Font = struct
  type glyph_placement = [ `Aligned | `Subpixel ]

  type t = {
    glyphes: Stb_truetype.t;
    glyphes_id : int;
    size: float;
    blur: float;
    spacing: float;
    line_height: float;
    placement   : glyph_placement;
  }

  let make ?(size=16.0) ?(blur=0.0) ?(spacing=0.0) ?(line_height=1.0) ?(placement=`Aligned) glyphes =
    let glyphes_id = Oo.id (Obj.magic glyphes) in
    { glyphes; glyphes_id; blur; size; spacing; line_height; placement }

  type metrics = {
    ascent   : float;
    descent  : float;
    line_gap : float;
  }

  let font_metrics t =
    let scale = Stb_truetype.scale_for_pixel_height t.glyphes t.size in
    let {Stb_truetype. ascent; descent; line_gap} =
      Stb_truetype.vmetrics t.glyphes in
    { ascent = float ascent *. scale;
      descent = float descent *. scale;
      line_gap = float line_gap *. scale;
    }

  let text_width t text =
    let len = String.length text in
    let index = ref 0 in
    let width = ref 0 in
    let last = ref Stb_truetype.invalid_glyph in
    while !index < len  do
      match utf8_decode index text with
      | -1 -> last := Stb_truetype.invalid_glyph
      | cp ->
        let glyph = Stb_truetype.get t.glyphes cp in
        width := !width
                 + Stb_truetype.kern_advance t.glyphes !last glyph
                 + Stb_truetype.glyph_advance t.glyphes glyph;
        last := glyph
    done;
    (float !width *. Stb_truetype.scale_for_pixel_height t.glyphes t.size)

  type measure = {
    width : float;
    height : float;
    depth : float;
  }

  let text_measure t text =
    let len = String.length text in
    let index = ref 0 in
    let width = ref 0 in
    let ascent = ref 0 in
    let descent = ref 0 in
    let maxi a b : int = if a >= b then a else b in
    let mini a b : int = if a <= b then a else b in
    let last = ref Stb_truetype.invalid_glyph in
    while !index < len  do
      match utf8_decode index text with
      | -1 -> last := Stb_truetype.invalid_glyph
      | cp ->
        let glyph = Stb_truetype.get t.glyphes cp in
        let box = Stb_truetype.glyph_box t.glyphes glyph in
        ascent := maxi !ascent box.y1;
        descent := mini !descent box.y0;
        width := !width
                 + Stb_truetype.kern_advance t.glyphes !last glyph
                 + Stb_truetype.glyph_advance t.glyphes glyph;
        last := glyph
    done;
    let scale = Stb_truetype.scale_for_pixel_height t.glyphes t.size in
    { width  = float !width *. scale;
      height = float !ascent *. scale;
      depth  = float (- !descent) *. scale }
end

module Glyph = struct
  let decimal_quantize x = int_of_float (x *. 10.0)

  let subpixel_quantize x =
    let rec aux x n =
      if n >= x then n
      else aux x (n lsl 1)
    in
    (aux (int_of_float (x *. 1.70)) 4) * 10

  let estimate_scale sx sy {Font. size; placement}  =
    let factor = sqrt (sx *. sx +. sy *. sy) in
    let scale = factor *. size in
    match placement with
    | `Aligned ->
      let x = decimal_quantize scale in
      if x > 2000
      then (float x /. 2000.0 /. factor, 2000)
      else (1.0 /. factor, x)
    | `Subpixel ->
      let x = subpixel_quantize scale in
      ((scale /. (float x /. 10.0)) /. factor, x)

  type key = {
    cp    : int;
    scale : int;
    ttf   : Stb_truetype.t;
    ttf_id : int;
    blur  : int;
  }

  let key ~sx ~sy font =
    let ttf = font.Font.glyphes in
    let ttf_id = font.Font.glyphes_id in
    let blur = decimal_quantize font.Font.blur in
    let factor, scale = estimate_scale sx sy font in
    (factor, (fun cp -> { cp; scale; ttf; ttf_id; blur }))

  type cell = {
    box   : Stb_truetype.box;
    uv    : Stb_truetype.box;
    glyph : Stb_truetype.glyph;
    mutable frame : int;
  }
end

type font_buffer = {
  image: Stb_image.int8 Stb_image.t;
  texture: Texture.t;
  mutable room : unit Maxrects.t;
}

let null_cell =
  let null_box = {Stb_truetype. x0 = 0; y0 = 0; x1 = 0; y1 = 0} in
  {Glyph. box = null_box; uv = null_box;
   glyph = Stb_truetype.invalid_glyph; frame = -1 }

module Glyphtbl = Hashtbl.Make (struct 
  type t = Glyph.key

  let equal (a :t) (b:t) = 
    a.cp == b.cp && 
    a.blur == b.blur && 
    a.scale == b.scale && 
    a.ttf_id == b.ttf_id

  let hash (a :t) = 
    let hash = a.cp in
    let hash = Int.logxor (hash * 0x1f1f1f1f) a.blur  in 
    let hash = Int.logxor (hash * 0x45d9f3b0) a.scale in 
    let hash = Int.logxor (hash * 0x119de1f3) a.ttf_id in 
    hash

end)
type font_stash = {
  font_glyphes: Glyph.cell Glyphtbl.t;
  font_todo: unit Glyphtbl.t;
  mutable font_buffer: font_buffer option;
}


let font_stash () = {
  font_glyphes = Glyphtbl.create 8;
  font_todo = Glyphtbl.create 8;
  font_buffer = None;
}

let align_place factor x =
  let x = x +. factor *. 0.5 in x -. mod_float x factor

let place factor = function
  | `Subpixel -> (fun x -> x)
  | `Aligned -> align_place factor

let render_glyphes stash _ xform (font,pos,text) quad ~(push : unit -> unit) =
  let x = Gg.P2.x pos and y = Gg.P2.y pos in
  let glyphes = font.Font.glyphes in
  let scale = Stb_truetype.scale_for_pixel_height glyphes font.Font.size in
  let factor, key = Glyph.key
      ~sx:(Transform.scale_x xform)
      ~sy:(Transform.scale_y xform)
      font
  in
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
      match Glyphtbl.find stash.font_glyphes key with
      | cell when cell == null_cell ->
        last := Stb_truetype.invalid_glyph
      | { Glyph. box; uv; glyph; _ } ->
        let open Stb_truetype in
        xoff := !xoff + Stb_truetype.kern_advance glyphes !last glyph;
        last := glyph;
        (*Printf.eprintf
          "character { x0 = %d; y0 = %d; x1 = %d; y1 = %d }, factor %.02fx\n%!"
          box.x0 box.y0 box.x1 box.y1 factor;*)
        let x = place (x +. float !xoff *. scale) in
        let open Typesetter in
        quad.x0 <- x +. float (box.x0 - 2) *. factor;
        quad.y0 <- y +. float (box.y0 - 2) *. factor;
        quad.x1 <- x +. float (box.x1 + 2) *. factor;
        quad.y1 <- y +. float (box.y1 + 2) *. factor;
        quad.u0 <- float (uv.x0 - 2) *. ibufsize;
        quad.v0 <- float (uv.y0 - 2) *. ibufsize;
        quad.u1 <- float (uv.x1 + 2) *. ibufsize;
        quad.v1 <- float (uv.y1 + 2) *. ibufsize;
        push ();
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

let new_font_buffer renderer width height =
  let data = Bigarray.(Array1.create int8_unsigned c_layout (width * height)) in
  Bigarray.Array1.fill data 0;
  let image = ok (Stb_image.image ~width ~height ~channels:1 data) in
  let texture = Texture.from_image renderer ~name:"font atlas" image in
  let room = Maxrects.add_bin () width height Maxrects.empty in
  { image; texture; room }

let box_offset {Stb_truetype. x0; x1; y0; y1 } p =
  {Stb_truetype. x0 = x0 - p; x1 = x1 + p; y0 = y0 - p; y1 = y1 + p }

let frame_nr = ref 0

let padding = 3

let bake_glyphs renderer t =
  let buffer = match t.font_buffer with
    | Some buffer -> buffer
    | None ->
      let buffer = new_font_buffer renderer bufsize bufsize in
      t.font_buffer <- Some buffer;
      buffer
  in
  let add_box ({ Glyph. scale; cp; ttf; blur } as key) () boxes =
    match Stb_truetype.find ttf cp with
    | None ->
      Glyphtbl.add t.font_glyphes key null_cell;
      boxes
    | Some glyph ->
      let scale = Stb_truetype.scale_for_pixel_height ttf (float scale /. 10.0) in
      let box = Stb_truetype.get_glyph_bitmap_box ttf glyph ~scale_x:scale ~scale_y:scale in
      let {Stb_truetype. x0; y0; x1; y1} = box in
      let blur_pad = (blur + 9) / 10 in
      let pad = (padding + blur_pad) * 2 in
      let box =
        Maxrects.box
          (key, ttf, glyph, scale, box)
          (x1 - x0 + pad)
          (y1 - y0 + pad)
      in
      box :: boxes
  in
  let todo = Glyphtbl.fold add_box t.font_todo [] in
  let room, boxes = Maxrects.insert_batch buffer.room todo in
  let room, boxes =
    if List.exists (function None -> true | _ -> false) boxes then (
      let todo = Glyphtbl.fold
          (fun key cell todo ->
             if cell.Glyph.frame = !frame_nr then add_box key () todo else todo)
          t.font_glyphes todo
      in
      Glyphtbl.reset t.font_glyphes;
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
        let pad = padding + (key.Glyph.blur + 9) / 20 in
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
            let uv = {Stb_truetype. x0 = x; x1 = x + w - 1;
                      y0 = y; y1 = y + h - 1} in
            let box = box_offset box pad in
            Stb_truetype.blur_glyph_bitmap
              buffer.image.Stb_image.data
              ~width:buffer.image.Stb_image.width
              ~height:buffer.image.Stb_image.height
              uv
              (float key.Glyph.blur /. 10.0);
            uv, box
          )
        in
        Glyphtbl.add t.font_glyphes key { Glyph. box; uv; frame = !frame_nr; glyph }
    ) boxes;
  Glyphtbl.reset t.font_todo;
  Texture.update buffer.texture buffer.image;
  incr frame_nr

let has_todo stash = Glyphtbl.length stash.font_todo > 0

let allocate_glyphes stash renderer ~sx ~sy (font,_pos,text) =
  let _, key = Glyph.key sx sy font in
  let r = ref 0 in
  let len = String.length text in
  let frame_nr = !frame_nr in
  let has_todo0 = has_todo stash in
  while !r < len do
    match utf8_decode r text with
    | -1 -> ()
    | cp ->
      let key = key cp in
      match Glyphtbl.find stash.font_glyphes key with
      | cache -> cache.Glyph.frame <- frame_nr
      | exception Not_found ->
        if not (Glyphtbl.mem stash.font_todo key) then
          (*(prerr_endline ("new glyph: " ^ string_of_int cp);*)
          (Glyphtbl.add stash.font_todo key ())
  done;
  if not has_todo0 && (has_todo stash) then
    Some (fun () -> bake_glyphs renderer stash)
  else
    None

type simple_typesetter = (Font.t * Gg.p2 * string) typesetter

let simple_typesetter () =
  let stash = font_stash () in
  Wall.Typesetter.make
    ~allocate:(allocate_glyphes stash)
    ~render:(render_glyphes stash)

let a_simple_typesetter = lazy (simple_typesetter ())

let simple_text
    ?(typesetter=Lazy.force a_simple_typesetter)
    ?(halign=`LEFT) ?(valign=`BASELINE) font ~x ~y str
  =
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
  Image.typeset typesetter (font, Gg.P2.v x y, str)

