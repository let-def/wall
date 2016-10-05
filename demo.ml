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

open Wall
module C = Wall_canvas

let draw_eyes vg xf x y w h mx my t =
  let ex = w *. 0.23 in
  let ey = h *. 0.5 in
  let lx = x +. ex in
  let ly = y +. ey in
  let rx = x +. w -. ex in
  let ry = y +. ey in
  let br = min ex ey *. 0.5 in
  let blink = 1.0 -. (sin (t *. 0.5) ** 200.0) *. 0.8 in
  C.new_path vg xf;
  C.ellipse vg ~cx:(lx +. 3.0) ~cy:(ly +. 16.0) ~rx:ex ~ry:ey;
  C.ellipse vg ~cx:(rx +. 3.0) ~cy:(ry +. 16.0) ~rx:ex ~ry:ey;
  C.fill vg (Paint.linear_gradient
               ~sx:x ~sy:(y +. h *. 0.5) ~ex:(x +. w *. 0.1) ~ey:(y +. h)
               ~inner:(Color.v 0.0 0.0 0.0 0.125)
               ~outer:(Color.v 0.0 0.0 0.0 0.0625));
  C.new_path vg xf;
  C.ellipse vg ~cx:lx ~cy:ly ~rx:ex ~ry:ey;
  C.ellipse vg ~cx:rx ~cy:ry ~rx:ex ~ry:ey;
  C.fill vg (Paint.linear_gradient
               ~sx:x ~sy:(y +. h *. 0.25) ~ex:(x +. w *. 0.1) ~ey:(y +. h)
               ~inner:(Color.v 0.86 0.86 0.86 1.0)
               ~outer:(Color.v 0.5 0.5 0.5 1.0));
  let norm (dx, dy) =
    let d = 1.0 /. (max 1.0 (sqrt (dx *. dx +. dy *. dy))) in
    (dx *. d, dy *. d)
  in
  let dx, dy = norm ((mx -. rx) /. (ex *. 10.0), (my -. ry) /. (ey *. 10.0)) in
  let dx = dx *. ex *. 0.4 in
  let dy = dy *. ey *. 0.5 in
  C.new_path vg xf;
  C.ellipse vg ~cx:(lx +. dx) ~cy:(ly +. dy +. ey *. 0.25 *. (1.0 -. blink)) ~rx:br ~ry:(br *. blink);
  C.fill vg (Paint.color (Color.v 0.0625 0.0625 0.0625 1.0));
  let gloss = Paint.radial_gradient
      ~cx:(rx -. ex *. 0.25) ~cy:(ry -. ey *. 0.5)
      ~inr:(ex *. 0.1) ~outr:(ex *. 0.75)
      ~inner:(Color.v 1.0 1.0 1.0 0.5)
      ~outer:(Color.v 1.0 1.0 1.0 0.0)
  in
  C.new_path vg xf;
  C.ellipse vg ~cx:lx ~cy:ly ~rx:ex ~ry:ey;
  C.fill vg gloss;
  C.new_path vg xf;
  C.ellipse vg ~cx:rx ~cy:ry ~rx:ex ~ry:ey;
  C.fill vg gloss

let draw_graph vg xf x y w h t =
  let samples = [|
    (1.0 +. sin (t *. 1.2345  +. cos (t *. 0.33457) *. 0.44 )) *. 0.5;
    (1.0 +. sin (t *. 0.68363 +. cos (t *. 1.3    ) *. 1.55 )) *. 0.5;
    (1.0 +. sin (t *. 1.1642  +. cos (t *. 0.33457) *. 1.24 )) *. 0.5;
    (1.0 +. sin (t *. 0.56345 +. cos (t *. 1.63   ) *. 0.14 )) *. 0.5;
    (1.0 +. sin (t *. 1.6245  +. cos (t *. 0.254  ) *. 0.3  )) *. 0.5;
    (1.0 +. sin (t *. 0.345   +. cos (t *. 0.03   ) *. 0.6  )) *. 0.5;
  |] in
  let dx = w /. 5.0 in
  let sx i = x +. float i *. dx in
  let sy i = y +. h *. samples.(i) *. 0.8 in
  C.new_path vg xf;
  C.move_to vg ~x:(sx 0) ~y:(sy 0);
  for i = 1 to 5 do
    C.bezier_to vg
      ~c1x:(sx (i - 1) +. dx *. 0.5) ~c1y:(sy (i - 1))
      ~c2x:(sx i +. dx *. 0.5) ~c2y:(sy i)
      ~x:(sx i) ~y:(sy i)
  done;
  C.line_to vg ~x:(x +. w) ~y:(y +. h);
  C.line_to vg ~x ~y:(y +. h);
  C.fill vg (Paint.linear_gradient ~sx:x ~sy:y ~ex:x ~ey:(y +. h)
               ~inner:(Color.v 0.00 0.60 0.75 0.00)
               ~outer:(Color.v 0.00 0.60 0.75 0.25));

  (* Graph line *)
  C.new_frame vg;
  C.move_to vg (sx 0) (sy 0 +. 2.0);
  for i = 1 to 5 do
    C.bezier_to vg
      ~c1x:(sx (i - 1) +. dx *. 0.5) ~c1y:(sy (i - 1) +. 2.0)
      ~c2x:(sx i -. dx *. 0.5) ~c2y:(sy i +. 2.0)
      ~x:(sx i +. 2.0) ~y:(sy i +. 2.0)
  done;
  C.stroke vg
    (Paint.color (Color.v 0.0 0.0 0.0 0.125))
    {Outline.default with Outline.stroke_width = 3.0};
  C.new_path vg xf;
  C.move_to vg (sx 0) (sy 0);
  for i = 1 to 5 do
    C.bezier_to vg
      ~c1x:(sx (i - 1) +. dx *. 0.5) ~c1y:(sy (i - 1))
      ~c2x:(sx i -. dx *. 0.5) ~c2y:(sy i)
      ~x:(sx i) ~y:(sy i)
  done;
  C.stroke vg
    (Paint.color (Color.v 0.0 0.60 0.75 1.0))
    {Outline.default with Outline.stroke_width = 3.0};
  (* Graph sample pos *)
  for i = 0 to 5 do
    C.new_path vg xf;
    C.rect vg ~x:(sx i -. 10.0) ~y:(sy i -. 10.0 +. 2.0) ~w:20.0 ~h:20.0;
    C.fill vg
      (Paint.radial_gradient ~cx:(sx i) ~cy:(sy i +. 2.0) ~inr:3.0 ~outr:8.0
         ~inner:(Color.v 0.0 0.0 0.0 0.125) ~outer:Color.black)
  done;
  C.new_path vg xf;
  for i = 0 to 5 do
    C.circle vg ~cx:(sx i) ~cy:(sy i) ~r:4.0;
  done;
  C.fill vg (Paint.color (Color.v 0.0 0.6 0.75 1.0));
  C.new_path vg xf;
  for i = 0 to 5 do
    C.circle vg ~cx:(sx i) ~cy:(sy i) ~r:2.0;
  done;
  C.fill vg (Paint.color (Color.v 0.8 0.8 0.8 1.0))

let draw_spinner vg xf cx cy r t =
  let a0 = 0.0 +. t *. 6.0 in
  let a1 = C.pi +. t *. 6.0 in
  let r0 = r in
  let r1 = r *. 0.75 in
  C.new_path vg xf;
  C.arc vg ~cx ~cy ~r:r0 ~a0:a0 ~a1:a1 `CW;
  C.arc vg ~cx ~cy ~r:r1 ~a0:a1 ~a1:a0 `CCW;
  C.close_path vg;
  let sx = cx +. cos a0 *. (r0 +. r1) *. 0.5 in
  let sy = cy +. sin a0 *. (r0 +. r1) *. 0.5 in
  let ex = cx +. cos a1 *. (r0 +. r1) *. 0.5 in
  let ey = cy +. sin a1 *. (r0 +. r1) *. 0.5 in
  C.fill vg
    (Paint.linear_gradient ~sx ~sy ~ex ~ey
       ~inner:(Color.v 0.0 0.0 0.0 0.0)
       ~outer:(Color.v 0.0 0.0 0.0 0.5))

let draw_demo vg xf mx my w h t =
  draw_eyes vg xf (w -. 250.0) 50.0 150.0 100.0 mx my t;
  draw_graph vg xf 0.0 (h /. 2.0) w (h /. 2.0) t

open Tsdl

let () =
  ignore (Sdl.init Sdl.Init.video);
  let vg = Wall_gl.create ~antialias:true ~stencil_strokes:bool ~debug:false in

