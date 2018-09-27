let perlin_noise3 =
  let t = [|0x15; 0x38; 0x32; 0x2c; 0x0d; 0x13; 0x07; 0x21|] in
  let b2 n b = (n lsr b) land 1 in
  let b i j k b =
    let idx = (b2 i b lsl 2) lor (b2 j b lsl 1) lor (b2 k b) in
    t.(idx)
  in
  let shuffle i j k =
    b i j k 0 + b j k i 1 + b k i j 2 + b i j k 3 +
    b j k i 4 + b k i j 5 + b i j k 6 + b j k i 7
  in
  let select c l r = if c then l else r in
  fun x y z ->
    let a = Array.make 3 0 in
    let s = (x +. y +. z) *. (1.0 /. 3.0) in
    let i = floor (x +. s) in
    let j = floor (y +. s) in
    let kk = floor (z +. s) in
    let s = (i +. j +. kk) *. (1.0 /. 6.0) in
    let u = x -. i  +. s in
    let v = y -. j  +. s in
    let w = z -. kk +. s in
    let i = int_of_float i in
    let j = int_of_float j in
    let kk = int_of_float kk in
    let k aa =
      let s = float (a.(0) + a.(1) + a.(2)) /. 6.0 in
      let x = u -. float a.(0) +. s in
      let y = v -. float a.(1) +. s in
      let z = w -. float a.(2) +. s in
      let t = 0.6 -. x *. x -. y *. y -. z *. z in
      let h = shuffle (i + a.(0)) (j + a.(1)) (kk + a.(2)) in
      a.(aa) <- a.(aa) + 1;
      if t < 0.0 then 0.0 else
        let b5 = (h lsr 5) land 1 in
        let b4 = (h lsr 4) land 1 in
        let b3 = (h lsr 3) land 1 in
        let b2 = (h lsr 2) land 1 in
        let bb = h land 3 in
        let p = ref x in
        let q = ref y in
        let r = ref z in
        if bb = 2 then (
          p := y;
          q := z;
          r := x;
        ) else if bb = 3 then (
          p := z;
          q := x;
          r := y;
        );
        if b5 = b3 then
          p := -. !p;
        if b5 = b4 then
          q := -. !q;
        if b5 <> (b4 lxor b3) then
          r := -. !r;
        let t = t *. t in
        let tmp1 =
          if bb = 0 then !q +. !r
          else if b2 = 0 then !q
          else !r
        in
        8.0 *. t *. t *. (!p +. tmp1)
    in
    let hi = select (w < u) (select (v < u) 0 1) (select (w < v) 1 2) in
    let lo = select (u < w) (select (u < v) 0 1) (select (v < w) 1 2) in
    k hi +. k (3 - hi - lo) +. k lo +. k 0

let perlin_noise2 x y = perlin_noise3 x y 0.0

let grid_width = 33
let grid_height = 19

let grid = Array.create_float (grid_width * grid_height * 2)

let fill_noise t =
  for x = 0 to grid_width - 2 do
    for y = 0 to grid_height - 2 do
      let a = perlin_noise2 (float x *. 0.1 +. t) (float y *. 0.1 -. t) in
      let b = perlin_noise2 (float y *. 0.1 +. t) (float x *. 0.1 -. t) in
      grid.((x * grid_height + y) * 2 + 0) <- a;
      grid.((x * grid_height + y) * 2 + 1) <- b;
    done
  done

let g_a x y =
  grid.((x * grid_height + y) * 2 + 0)

let g_b x y =
  grid.((x * grid_height + y) * 2 + 1)

let g_dx x y = if x > 0 && x < grid_width - 1 then g_a x y *. 3.0 else 0.0
let g_dy x y = if y > 0 && y < grid_height - 1 then g_b x y *. 3.0 else 0.0
let g_cr x y = 0.5 +. g_a x y *. 2.0
let g_cg x y = 0.5 +. g_b x y *. 2.0
let g_cb x y = 0.5 -. g_a x y -. g_b x y

let g_color x y =
  let a = g_a x y and b = g_b x y in
  let r = 0.5 +. a *. 2.0 in
  let g = 0.5 +. b *. 2.0 in
  let b = (0.5 -. a -. b) *. 2.0 in
  Wall.Color.v r g b 1.0

let g_x x y = float x +. g_dx x y
let g_y x y = float y +. g_dy x y

open Wall

let cell x y =
  let path =
    Path.make @@ fun ctx ->
    Path.move_to ctx (g_x x y) (g_y x y);
    Path.line_to ctx (g_x (x+1) y) (g_y (x+1) y);
    Path.line_to ctx (g_x (x+1) (y+1)) (g_y (x+1) (y+1));
    Path.line_to ctx (g_x x (y+1)) (g_y x (y+1))
  in
  let bg =
    Image.stack
      (Image.paint (Paint.color (g_color x y)) (Image.fill path))
      (Image.paint Paint.black (Image.stroke (Outline.make ~width:0.01 ()) path))
  in
  if x > 0 && y > 0 then
    Image.stack bg
      (Image.paint Paint.white
         (Image.fill_path
            (Path.circle ~cx:(g_x x y) ~cy:(g_y x y)
               ~r:((abs_float (g_dx x y) +. abs_float (g_dy x y)) *. 0.25))))
  else
    bg

let frame t =
  fill_noise t;
  let image = ref Image.empty in
  for x = 0 to grid_width - 2 do
    for y = 0 to grid_height - 2 do
      image := Image.stack !image (cell x y)
    done
  done;
  Image.transform (Transform.scale 20.0 20.0) !image

open Tsdl
open Tgles2

let main () =
  Printexc.record_backtrace true;
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
    match
      Sdl.create_window ~w:640 ~h:480 "SDL OpenGL"
        Sdl.Window.(opengl + allow_highdpi)
    with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      (*Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;*)
      (*Sdl.gl_set_attribute Sdl.Gl.context_major_version 2;*)
      (*Sdl.gl_set_attribute Sdl.Gl.context_minor_version 1;*)
      ignore (Sdl.gl_set_swap_interval (-1));
      let ow, oh = Sdl.gl_get_drawable_size w in
      match Sdl.gl_create_context w with
      | Error (`Msg e) -> Sdl.log "Create context error: %s" e; exit 1
      | Ok ctx ->
        let context = Renderer.create ~antialias:true () in
        let quit = ref false in
        let event = Sdl.Event.create () in
        while not !quit do
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
          Renderer.render context ~width:640.0 ~height:480.0 (frame (Sys.time ()));
          Sdl.gl_swap_window w;
        done;
        Sdl.gl_delete_context ctx;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()
