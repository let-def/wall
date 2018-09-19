[@@@ocaml.warning "-6-9-27"]
open Tsdl
open Tgles2

let (>>=) x f = match x with
  | Ok a -> f a
  | Error x as result -> result

let on_failure ~cleanup result =
  begin match result with
    | Ok _ -> ()
    | Error _ -> cleanup ()
  end;
  result

let get_result = function
  | Ok x -> x
  | Error (`Msg msg) -> failwith msg

let initialized = lazy (Sdl.init Sdl.Init.video)

type state = {
  time: float
}

type slide = state -> Wall.image list

let ticks () =
  Int32.to_int (Sdl.get_ticks ())

type window = {
  win: Sdl.window;
  gl: Sdl.gl_context;
  wall: Wall.renderer;
  event: Sdl.event;
  mutable quit: bool;
  mutable running_since: int option;
  mutable prev_slides : slide list;
  mutable next_slides : slide list;
  mutable time_acc: float;
  mutable fullscreen: bool;
}

let make_window ~w ~h =
  Lazy.force initialized >>= fun () ->
  Sdl.create_window ~w ~h "Slideshow"
    Sdl.Window.(opengl + allow_highdpi + resizable + hidden)
  >>= fun win ->
  ignore (Sdl.gl_set_swap_interval (-1));
  ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
  on_failure (
    Sdl.gl_create_context win >>= fun gl ->
    let wall = Wall.Renderer.create ~antialias:true ~stencil_strokes:true () in
    Ok { win; gl; wall; event = Sdl.Event.create ();
         prev_slides = []; next_slides = [];
         quit = false; running_since = None; time_acc = 0.0; fullscreen = false }
  ) ~cleanup:(fun () -> Sdl.destroy_window win)

let get_time t =
  match t.running_since with
  | None -> t.time_acc
  | Some tick0 -> t.time_acc +. float (ticks () - tick0) /. 1000.0

let set_pause t pause =
  if pause then (
    t.time_acc <- get_time t;
    t.running_since <- None;
  ) else (
    t.running_since <- Some (ticks ())
  )

let reset_time t =
  t.time_acc <- 0.0;
  set_pause t false

let set_slides t slides =
  let rec select_slides acc prevs nexts =
    match prevs, nexts with
    | (_ :: prevs'), (next :: nexts') ->
      select_slides (next :: acc) prevs' nexts'
    | _, _ -> acc, nexts
  in
  let prev_slides, next_slides = select_slides [] t.prev_slides slides in
  t.prev_slides <- prev_slides;
  t.next_slides <- next_slides

let render_slide t slide =
  Sdl.gl_make_current t.win t.gl >>= fun () ->
  let (width, height) as physical_size = Sdl.gl_get_drawable_size t.win in
  Gl.viewport 0 0 width height;
  Gl.clear_color 0.0 0.0 0.0 1.0;
  Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
  Gl.enable Gl.blend;
  Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha;
  Gl.enable Gl.cull_face_enum;
  Gl.disable Gl.depth_test;
  let width = float width and height = float height in
  let transform =
    let r = 1024.0 /. 768.0 in
    let r' = width /. height in
    let r =
      if r' > r then
        (height /. 768.0)
      else
        (width /. 1024.0)
    in
    let x = (width -. 1024.0 *. r) /. 2.0 in
    let y = (height -. 768.0 *. r) /. 2.0 in
    Wall.Transform.rescale r r
      (Wall.Transform.translation x y)
    (*Wall.Transform.translate ~x ~y (Wall.Transform.scale r r)*)
  in
  Wall.Renderer.render t.wall ~width ~height
    (Wall.Image.transform transform slide);
  Sdl.gl_swap_window t.win;
  Ok ()

let process_events t =
  while Sdl.poll_event (Some t.event) do
    let run_action = function
      | `Quit -> t.quit <- true
      | `Prev ->
        begin match t.prev_slides with
          | x :: xs ->
            t.next_slides <- x :: t.next_slides;
            t.prev_slides <- xs;
            reset_time t
          | [] -> ()
        end
      | `Next ->
        begin match t.next_slides with
          | x :: xs ->
            t.prev_slides <- x :: t.prev_slides;
            t.next_slides <- xs;
            reset_time t
          | [] -> ()
        end
      | `Pause -> set_pause t (t.running_since <> None)
      | `Fullscreen ->
        t.fullscreen <- not t.fullscreen;
        ignore (Sdl.show_cursor (not t.fullscreen) : _ result);
        ignore (Sdl.set_window_fullscreen t.win
                  (if t.fullscreen
                   then Sdl.Window.fullscreen_desktop
                   else Sdl.Window.windowed)
                  : _ result)
    in
    let bindings = [
      (Sdl.K.[q], `Quit);
      (Sdl.K.[p], `Pause);
      (Sdl.K.[left; up], `Prev);
      (Sdl.K.[right; down], `Next);
      (Sdl.K.[f], `Fullscreen);
    ]
    in
    match Sdl.Event.enum (Sdl.Event.get t.event Sdl.Event.typ) with
    | `Key_up ->
      let key = Sdl.Event.get t.event Sdl.Event.keyboard_keycode in
      begin match List.find (fun (keys, _) -> List.mem key keys) bindings with
        | exception Not_found -> ()
        | (_, action) -> run_action action
      end
    | `Quit -> run_action `Quit
    | _ -> ()
  done;
  let slide = match t.next_slides with
    | slide :: _ -> Wall.Image.seq (slide {time = get_time t})
    | [] -> Wall.Image.empty
  in
  match render_slide t slide with
  | Result.Ok () -> ()
  | Result.Error (`Msg msg) ->
    prerr_endline ("Render error: " ^ msg)

let destroy_window { win; gl; wall } =
  Wall.Renderer.delete wall;
  Sdl.gl_delete_context gl;
  Sdl.destroy_window win

let window =
  get_result (make_window ~w:1024 ~h:768)

let unix_stat fname =
  match Unix.stat fname with
  | stat -> {stat with Unix.st_atime = stat.Unix.st_mtime}
  | exception (Unix.Unix_error (Unix.ENOENT, _, _)) ->
    raise Not_found

let auto_reload names =
  let update fname =
    let stat' = Some (unix_stat fname) in
    Mod_use.mod_use fname;
    stat'
  in
  let rec refresh stats names =
    match stats, names with
    | (stat :: stats'), (name :: names') when stat = Some (unix_stat name) ->
      stat :: refresh stats' names'
    | _ -> List.map update names
  in
  let stats = ref [] in
  Sdl.show_window window.win;
  window.quit <- false;
  while not window.quit do
    process_events window;
    begin try
        stats := refresh !stats names;
      with Not_found -> ()
    end;
    (*Unix.sleepf 0.02*)
  done;
  Sdl.hide_window window.win
