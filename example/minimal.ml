open Tsdl

open Wall
module C = Wall_canvas
module P = C.Path

let load_font name =
  let ic = open_in_bin name in
  let dim = in_channel_length ic in
  let fd = Unix.descr_of_in_channel ic in
  let buffer = Bigarray.Array1.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false dim in
  let offset = List.hd (Stb_truetype.enum buffer) in
  match Stb_truetype.init buffer offset with
  | None -> assert false
  | Some font -> font

let font_sans = lazy (load_font "Roboto-Regular.ttf")

let normalize (dx, dy) =
  let d = sqrt (dx *. dx +. dy *. dy) in
  if d > 1.0 then
    (dx /. d, dy /. d)
  else
    (dx, dy)

let w = 1000
let h = 600
let f = (try float_of_string Sys.argv.(1) with _ -> 1.0)
let fw = int_of_float (f *. float w)
let fh = int_of_float (f *. float h)

let render context sw sh t =
  let lw = float w in
  let lh = float h in
  let pw = lw *. f *. sw in
  let ph = lh *. f *. sh in
  let vg = C.new_frame context (Gg.V2.v pw ph) in
  C.draw' vg
    Transform.identity
    (Paint.rgba 0.0 1.0 0.0 0.5)
    (C.stroke_path {Outline.default with Outline.stroke_width = 60.0} @@
     fun p -> P.rect p 300.0 100.0 50.0 50.0);
  let character =
    Char.chr @@
    let x = int_of_float (t *. 10.0) mod 62 in
    if x < 10 then Char.code '0' + x
    else if x < 36 then Char.code 'a' + x - 10
    else Char.code 'A' + x - 36
  in
  let size = 1.0 +. sin (t /. 10.0) *. 10.0 in
  let size = size *. size in
  C.text' vg ~halign:`CENTER ~valign:`MIDDLE ~x:100.0 ~y:100.0
    Transform.identity Paint.white
    (Font.make (Lazy.force font_sans) ~size)
    (String.make 1 character);
  C.flush_frame context

open Tgles2

let main () =
  Printexc.record_backtrace true;
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    match
      Sdl.create_window ~w:fw ~h:fh "SDL OpenGL"
        Sdl.Window.(opengl + allow_highdpi)
    with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      (*Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;*)
      (*Sdl.gl_set_attribute Sdl.Gl.context_major_version 2;*)
      (*Sdl.gl_set_attribute Sdl.Gl.context_minor_version 1;*)
      ignore (Sdl.gl_set_swap_interval (-1));
      let ow, oh = Sdl.gl_get_drawable_size w in
      Sdl.log "window size: %d,%d\topengl drawable size: %d,%d" fw fh ow oh;
      let sw = float ow /. float fw and sh = float oh /. float fh in
      ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
      match Sdl.gl_create_context w with
      | Error (`Msg e) -> Sdl.log "Create context error: %s" e; exit 1
      | Ok ctx ->
        let context = C.create_gl ~antialias:false ~stencil_strokes:false () in
        let quit = ref false in
        let event = Sdl.Event.create () in
        while not !quit do
          while Sdl.poll_event (Some event) do
            match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
            | `Quit -> quit := true
            | _ -> ()
          done;
          Gl.viewport 0 0 fw fh;
          Gl.clear_color 0.3 0.3 0.32 1.0;
          Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
          Gl.enable Gl.blend;
          Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha;
          Gl.enable Gl.cull_face_enum;
          Gl.disable Gl.depth_test;
          render context sw sh (Int32.to_float (Sdl.get_ticks ()) /. 1000.0);
          Sdl.gl_swap_window w;
        done;
        Sdl.gl_delete_context ctx;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()
