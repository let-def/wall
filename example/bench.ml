open! Tsdl
open! Tgles2

open! Wall
module I = Image
module P = Path
module Text = Wall_text

module type S = sig 
  type state 
  val init  : Wall.Renderer.t -> state
  val frame : state -> width:float -> height:float -> elapsed_seconds:float -> I.t
end

let load_font name =
  let ic = open_in_bin name in
  let dim = in_channel_length ic in
  let fd = Unix.descr_of_in_channel ic in
  let buffer =
    Unix.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false [|dim|]
    |> Bigarray.array1_of_genarray
  in
  let offset = List.hd (Stb_truetype.enum buffer) in
  match Stb_truetype.init buffer offset with
  | None -> assert false
  | Some font -> font

let font_sans = lazy (load_font "Roboto-Regular.ttf")

let run (module T: S) =
  let window_width = 1000 in
  let window_height = 800 in
  Printexc.record_backtrace true;
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    ignore (Sdl.gl_set_attribute Sdl.Gl.depth_size 24 : _ result);
    ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 8 : _ result);
    match
      Sdl.create_window ~w:window_width ~h:window_height "SDL OpenGL"
        Sdl.Window.(opengl + allow_highdpi)
    with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      (*Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;
        Sdl.gl_set_attribute Sdl.Gl.context_major_version 2;
        Sdl.gl_set_attribute Sdl.Gl.context_minor_version 1;*)
      ignore (Sdl.gl_set_swap_interval (-1));
      let ow, oh = Sdl.gl_get_drawable_size w in
      Sdl.log "window size: %d,%d\topengl drawable size: %d,%d" window_width window_height ow oh;
      let _sw = float ow /. float window_width and _sh = float oh /. float window_height in
      ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
      match Sdl.gl_create_context w with
      | Error (`Msg e) -> Sdl.log "Create context error: %s" e; exit 1
      | Ok ctx ->
        let context = Renderer.create ~antialias:true () in
        let state = T.init context in
        let quit = ref false in
        let event = Sdl.Event.create () in
        let prev_frame_fps = ref 0.0  in
        let freq = Sdl.get_performance_frequency () in
        let font_sans = Lazy.force font_sans in
        while not !quit do
          let timing_start = Sdl.get_performance_counter () in
          while Sdl.poll_event (Some event) do
            match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
            | `Quit -> quit := true
            | _ -> ()
          done;
          Gl.viewport 0 0 ow oh;
          Gl.clear_color 0.3 0.3 0.32 1.0;
          Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
          Gl.enable Gl.blend;
          Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha;
          Gl.enable Gl.cull_face_enum;
          Gl.disable Gl.depth_test;
          let elapsed_seconds = (Int32.to_float (Sdl.get_ticks ()) /. 1000.0) in
          let () = 
            let width = (float window_width) in 
            let height = (float window_height) in
            let image = T.frame state ~width ~height ~elapsed_seconds in
            let fps = 
              let fps = Printf.sprintf " FPS: %d" (Float.to_int !prev_frame_fps) in
              I.stack 
                (I.paint (Paint.color (Color.v 0.0 0.0 0.0 1.0))
                  Text.(simple_text
                     (Font.make ~blur:2.0 ~size:30.0 font_sans)
                     ~valign:`TOP ~halign:`LEFT
                     ~x:0.0 ~y:0.0 fps))
                (I.paint (Paint.color (Color.v 1.0 1.0 1.0 1.0))
                  Text.(simple_text
                     (Font.make ~size:30.0 font_sans)
                     ~valign:`TOP ~halign:`LEFT
                     ~x:0.0 ~y:0.0 fps))
            in
            Renderer.render context ~width ~height  (I.stack image fps)
          in
          Sdl.gl_swap_window w;
          let timing_end = Sdl.get_performance_counter () in
          let seconds_elapsed = Int64.to_float (Int64.sub timing_end timing_start) /. (Int64.to_float freq) in 
          prev_frame_fps := (1.0 /. seconds_elapsed);
          ()
        done;
        Sdl.gl_delete_context ctx;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0
