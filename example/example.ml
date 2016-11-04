open Tsdl

open Wall
module C = Wall_canvas

let normalize (dx, dy) =
  let d = sqrt (dx *. dx +. dy *. dy) in
  if d > 1.0 then
    (dx /. d, dy /. d)
  else
    (dx, dy)

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
  let dx, dy = normalize
      ((mx -. rx) /. (ex *. 10.0), (my -. ry) /. (ey *. 10.0)) in
  let dx = dx *. ex *. 0.4 in
  let dy = dy *. ey *. 0.5 in

  C.new_path vg xf;
  C.ellipse vg ~cx:(lx +. dx) ~cy:(ly +. dy +. ey *. 0.25 *. (1.0 -. blink))
    ~rx:br ~ry:(br *. blink);
  C.fill vg (Paint.color (Color.v 0.125 0.125 0.125 1.0));

  C.new_path vg xf;
  C.ellipse vg ~cx:(rx +. dx) ~cy:(ry +. dy +. ey *. 0.25 *. (1.0 -. blink))
    ~rx:br ~ry:(br *. blink);
  C.fill vg (Paint.color (Color.v 0.125 0.125 0.125 1.0));

  let gloss = Paint.radial_gradient
      ~cx:(lx -. ex *. 0.25) ~cy:(ry -. ey *. 0.5)
      ~inr:(ex *. 0.1) ~outr:(ex *. 0.75)
      ~inner:(Color.v 1.0 1.0 1.0 0.5)
      ~outer:(Color.v 1.0 1.0 1.0 0.0)
  in
  C.new_path vg xf;
  C.ellipse vg ~cx:lx ~cy:ly ~rx:ex ~ry:ey;
  C.fill vg gloss;

  let gloss = Paint.radial_gradient
      ~cx:(rx -. ex *. 0.25) ~cy:(ry -. ey *. 0.5)
      ~inr:(ex *. 0.1) ~outr:(ex *. 0.75)
      ~inner:(Color.v 1.0 1.0 1.0 0.5)
      ~outer:(Color.v 1.0 1.0 1.0 0.0)
  in
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
  (* Graph background *)
  C.new_path vg xf;
  C.move_to vg ~x:(sx 0) ~y:(sy 0);
  for i = 1 to 5 do
    C.bezier_to vg
      ~c1x:(sx (i - 1) +. dx *. 0.5) ~c1y:(sy (i - 1))
      ~c2x:(sx i -. dx *. 0.5) ~c2y:(sy i)
      ~x:(sx i) ~y:(sy i)
  done;
  C.line_to vg ~x:(x +. w) ~y:(y +. h);
  C.line_to vg ~x ~y:(y +. h);
  C.fill vg (Paint.linear_gradient ~sx:x ~sy:y ~ex:x ~ey:(y +. h)
               ~inner:(Color.v 0.00 0.60 0.75 0.00)
               ~outer:(Color.v 0.00 0.60 0.75 0.25));

  (* Graph line *)
  C.new_path vg xf;
  C.move_to vg (sx 0) (sy 0 +. 2.0);
  for i = 1 to 5 do
    C.bezier_to vg
      ~c1x:(sx (i - 1) +. dx *. 0.5) ~c1y:(sy (i - 1) +. 2.0)
      ~c2x:(sx i -. dx *. 0.5) ~c2y:(sy i +. 2.0)
      ~x:(sx i) ~y:(sy i +. 2.0)
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
         ~inner:(Color.v 0.0 0.0 0.0 0.125) ~outer:(Color.v 0.0 0.0 0.0 0.0))
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

let draw_spinner ?frame vg xf cx cy r t =
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
  C.fill ?frame vg
    (Paint.linear_gradient ~sx ~sy ~ex ~ey
       ~inner:(Color.v 0.0 0.0 0.0 0.0)
       ~outer:(Color.v 0.0 0.0 0.0 0.5))

let draw_colorwheel vg xf x y w h t =
  let cx = x +. w *. 0.5 in
  let cy = y +. h *. 0.5 in
  let hue = sin (t *. 0.12) in
  let r1 = min w h *. 0.5 -. 5.0 in
  let r0 = r1 -. 20.0 in
  let aeps = 0.5 /. r1 in
  for i = 0 to 5 do
    let a0 = float i /. 6.0 *. C.pi *. 2.0 -. aeps in
    let a1 = (float i +. 1.0) /. 6.0 *. C.pi *. 2.0 +. aeps in
    C.new_path vg xf;
    C.arc vg ~cx ~cy ~r:r0 ~a0:a0 ~a1:a1 `CW;
    C.arc vg ~cx ~cy ~r:r1 ~a0:a1 ~a1:a0 `CCW;
    C.close_path vg;
    let sx = cx +. cos a0 *. (r0 +. r1) *. 0.5 in
    let sy = cy +. sin a0 *. (r0 +. r1) *. 0.5 in
    let ex = cx +. cos a1 *. (r0 +. r1) *. 0.5 in
    let ey = cy +. sin a1 *. (r0 +. r1) *. 0.5 in
    (*Printf.printf "sx=%f, sy=%f, ex=%f, ey=%f\n%!" sx sy ex ey;*)
    C.fill vg (Paint.linear_gradient
                 ~sx ~sy ~ex ~ey
                 ~inner:(Color.hsl (a0 /. (2.0 *. C.pi)) 1.0 0.55)
                 ~outer:(Color.hsl (a1 /. (2.0 *. C.pi)) 1.0 0.55));
  done;
  C.new_path vg xf;
  C.circle vg ~cx ~cy ~r:(r0 -. 0.5);
  C.circle vg ~cx ~cy ~r:(r1 +. 0.5);
  C.stroke vg (Paint.color (Color.v 0.0 0.0 0.0 0.25))
    Outline.{default with stroke_width = 1.0};

  let xf = Transform.(rotate (hue *. 2.0 *. C.pi) (translate ~x:cx ~y:cy xf)) in
  (* Selector *)
  C.new_path vg xf;
  C.rect vg (r0 -. 1.0) (-3.0) (r1-.r0+.2.) 6.0;
  C.stroke vg (Paint.color (Color.gray ~a:0.75 1.0))
    Outline.{default with stroke_width = 2.0};

  C.new_path vg xf;
  C.rect vg ~x:(r0-.2.0-.10.0) ~y:(-.4.0-.10.0)
    ~w:(r1-.r0+.4.0+.20.0) ~h:(8.0+.20.0);
  C.rect vg ~x:(r0-.2.0) ~y:(-4.0) ~w:(r1-.r0+.4.0) ~h:8.0;
  C.set_winding vg `HOLE;
  C.fill vg (Paint.box_gradient ~x:(r0-.3.0) ~y:(-5.0)
               ~w:(r1-.r0+.6.0) ~h:10.0 ~r:2.0 ~f:4.0
               ~inner:(Color.gray ~a:0.5 0.0) ~outer:(Color.gray ~a:0.0 0.0));

  (* Center triangle *)
  let r = r0 -. 6.0 in
  let ax = cos (120.0/.180.0 *. C.pi) *. r in
  let ay = sin (120.0/.180.0 *. C.pi) *. r in
  let bx = cos (-.120.0/.180.0 *. C.pi) *. r in
  let by = sin (-.120.0/.180.0 *. C.pi) *. r in
  C.new_path vg xf;
  C.move_to vg r 0.0;
  C.line_to vg ax ay;
  C.line_to vg bx by;
  C.close_path vg;
  (*Printf.printf "sx=%f, sy=%f, ex=%f, ey=%f\n%!" r 0.0 ax ay;*)
  C.fill vg (Paint.linear_gradient ~sx:r ~sy:0.0 ~ex:ax ~ey:ay
               ~inner:(Color.hsl hue 1.0 0.5) ~outer:Color.white);
  C.fill vg (Paint.linear_gradient ~sx:((r+.ax)*.0.5) ~sy:((0.0+.ay)*.0.5)
               ~ex:bx ~ey:by
               ~inner:(Color.gray ~a:0.0 0.0) ~outer:(Color.gray ~a:1.0 0.0));
  C.stroke vg (Paint.color (Color.gray ~a:0.25 0.0)) Outline.default;

  (* Select circle on triangle *)
  let ax = cos (120.0/.180.0*.C.pi) *. r *. 0.3 in
  let ay = sin (120.0/.180.0*.C.pi) *. r *. 0.4 in
  C.new_path vg xf;
  C.circle vg ~cx:ax ~cy:ay ~r:5.0;
  C.stroke vg (Paint.color (Color.gray ~a:0.75 1.0))
    Outline.{default with stroke_width = 2.0};

  C.new_path vg xf;
  C.rect vg ~x:(ax -. 20.0) ~y:(ay -. 20.0) ~w:40.0 ~h:40.0;
  C.circle vg ~cx:ax ~cy:ay ~r:7.0;
  C.set_winding vg `HOLE;
  C.fill vg (Paint.radial_gradient ~cx:ax ~cy:ay ~inr:7.0 ~outr:9.0
               ~inner:(Color.gray ~a:0.25 0.0) ~outer:(Color.gray ~a:0.0 0.0))

let draw_lines vg xf x y w _h t =
  let pad = 5.0 in
  let s = w /. 9.0 -. pad *. 2.0 in
  let joins = [|`MITER; `ROUND; `BEVEL|] in
  let caps = [|`BUTT; `ROUND; `SQUARE|] in
  let px = function
    | 0 -> -.s*.0.25 +. cos (t*.0.3) *. s*.0.5
    | 1 -> -.s*.0.25
    | 2 -> s*.0.25
    | 3 -> s*.0.25 +. cos (-.t*.0.3) *. s*.0.5
    | _ -> assert false
  in
  let py = function
    | 0 -> sin (t*.0.3) *. s*.0.5
    | 1 -> 0.0
    | 2 -> 0.0
    | 3 -> sin (-.t*.0.3) *. s*.0.5
    | _ -> assert false
  in
  for i = 0 to 2 do
    for j = 0 to 2 do
      let fx = x +. s *. 0.5 +. float (i * 3 + j) /. 9.0 *. w +. pad in
      let fy = y -. s *. 0.5 +. pad in
      let px i = fx +. px i in
      let py i = fy +. py i in

      C.new_path vg xf;
      C.move_to vg (px 0) (py 0);
      C.line_to vg (px 1) (py 1);
      C.line_to vg (px 2) (py 2);
      C.line_to vg (px 3) (py 3);
      C.stroke vg (Paint.color (Color.gray ~a:0.625 0.0))
        Outline.{default with stroke_width = s *. 0.3;
                              line_cap = caps.(i);
                              line_join = joins.(j) };

      C.new_path vg xf;
      C.move_to vg (px 0) (py 0);
      C.line_to vg (px 1) (py 1);
      C.line_to vg (px 2) (py 2);
      C.line_to vg (px 3) (py 3);
      C.stroke vg (Paint.color (Color.v 0.0 0.75 1.0 1.0))
        Outline.{default with stroke_width = 1.0;
                              line_cap = `BUTT;
                              line_join = `BEVEL};
    done
  done

let draw_widths vg xf x y w =
  let paint = Paint.color Color.black in
  let y = ref y in
  for i = 0 to 19 do
    C.new_path vg xf;
    C.move_to vg x !y;
    C.line_to vg (x+.w) (!y+.w*.0.3);
    let stroke_width = (float i +. 0.5) /. 10.0 in
    C.stroke vg paint Outline.{default with stroke_width};
    y := !y +. 10.0;
  done

let draw_caps vg xf x y w =
  let caps = [| `BUTT; `ROUND; `SQUARE |] in
  let stroke_width = 8.0 in

  C.new_path vg xf;
  C.rect vg x y w 40.0;
  C.rect vg (x-.stroke_width/.2.0) y (w+.stroke_width) 40.0;
  C.fill vg (Paint.color (Color.gray ~a:0.125 1.0));

  for i = 0 to 2 do
    C.new_path vg xf;
    C.move_to vg x (y +. float (i * 10 + 5));
    C.line_to vg (x +. w) (y +. float (i * 10 + 5));
    C.stroke vg Paint.black
      Outline.{default with stroke_width; line_cap = caps.(i)};
  done

let draw_scissor vg xf x y t =
  let frame = Frame.default in
  let xf = Transform.(rotate (5.0 /. 180.0 *. C.pi) (translate ~x ~y xf)) in

  (* Draw first rect and set scissor to it's area. *)
  C.new_path vg xf;
  C.rect vg (-20.0) (-20.0) (60.0) (40.0);
  C.fill vg (Paint.color (Color.v 1.0 0.0 0.0 1.0));

  (* Draw second rectangle with offset and rotation. *)
  let frame = Frame.set_scissor (-20.0) (-20.0) 60.0 40.0 xf frame in
  let xf = Transform.(rotate t (translate 40.0 0.0 xf)) in

  (* Draw the intended second rectangle without any scissoring. *)
  C.new_path vg xf;
  C.rect vg (-20.0) (-10.0) 60.0 30.0;
  C.fill vg (Paint.color (Color.v 1.0 0.5 0.0 0.25));
  (* Draw second rectangle with combined scissoring. *)
  let frame = Frame.intersect_scissor (-20.0) (-10.0) 60.0 30.0 xf frame in
  C.fill vg ~frame (Paint.color (Color.v 1.0 0.5 0.0 1.0))

let draw_window vg xf title x y w h =
  let cornerRadius = 3.0 in
  (* Window *)
  C.new_path vg xf;
  C.round_rect vg x y w h cornerRadius;
  (* nvgFillColor(vg, nvgRGBA(0,0,0,128)); *)
  C.fill vg (Paint.color (Color.v 0.110 0.118 0.133 0.75));

  (* Drop shadow *)
  C.new_path vg xf;
  C.rect vg (x -. 10.0) (y -. 10.0) (w +. 20.0) (h +. 30.0);
  C.round_rect vg x y w h cornerRadius;
  C.set_winding vg `HOLE;
  C.fill vg (Paint.box_gradient x (y+.2.0) w h (cornerRadius*.2.0) 10.0
               (Color.gray ~a:0.5 0.0) (Color.gray ~a:0.0 0.0));

  (* Header *)
  C.new_path vg xf;
  C.round_rect vg (x+.1.0) (y+.1.0) (w-.2.0) 30.0 (cornerRadius -. 1.0);
  C.fill vg (Paint.linear_gradient x y x (y+.15.0)
               (Color.gray ~a:0.04 1.0) (Color.gray ~a:0.08 1.0));
  C.new_path vg xf;
  C.move_to vg (x+.0.5) (y+.0.5+.30.0);
  C.line_to vg (x+.0.5+.w-.1.0) (y+.0.5+.30.0);
  C.stroke vg (Paint.color (Color.gray ~a:0.125 0.0)) Outline.default;

  (* nvgFontSize(vg, 18.0f); *)
  (* nvgFontFace(vg, "sans-bold"); *)
  (* nvgTextAlign(vg,NVG_ALIGN_CENTER|NVG_ALIGN_MIDDLE); *)

  (* nvgFontBlur(vg,2); *)
  (* nvgFillColor(vg, nvgRGBA(0,0,0,128)); *)
  (* nvgText(vg, x+w/2,y+16+1, title, NULL); *)

  (* nvgFontBlur(vg,0); *)
  (* nvgFillColor(vg, nvgRGBA(220,220,220,160)); *)
  (* nvgText(vg, x+w/2,y+16, title, NULL); *)
  ()

let draw_searchbox vg xf text x y w h =
  let cornerRadius = h /. 2.0 -. 1.0 in
  (* Edit *)
  C.new_path vg xf;
  C.round_rect vg x y w h cornerRadius;
  C.fill vg (Paint.box_gradient x (y +. 1.5) w h (h /. 2.0) 5.0
               (Color.gray ~a:0.08 0.0) (Color.gray ~a:0.375 0.0));

  (* nvgBeginPath(vg);
     nvgRoundedRect(vg, x+0.5f,y+0.5f, w-1,h-1, cornerRadius-0.5f);
     nvgStrokeColor(vg, nvgRGBA(0,0,0,48));
     nvgStroke(vg);*)

  (* nvgFontSize(vg, h*1.3f); *)
  (* nvgFontFace(vg, "icons"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,64)); *)
  (* nvgTextAlign(vg,NVG_ALIGN_CENTER|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+h*0.55f, y+h*0.55f, cpToUTF8(ICON_SEARCH,icon), NULL); *)

  (* nvgFontSize(vg, 20.0f); *)
  (* nvgFontFace(vg, "sans"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,32)); *)

  (* nvgTextAlign(vg,NVG_ALIGN_LEFT|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+h*1.05f,y+h*0.5f,text, NULL); *)

  (* nvgFontSize(vg, h*1.3f); *)
  (* nvgFontFace(vg, "icons"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,32)); *)
  (* nvgTextAlign(vg,NVG_ALIGN_CENTER|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+w-h*0.55f, y+h*0.55f, cpToUTF8(ICON_CIRCLED_CROSS,icon), NULL); *)
  ()

let draw_dropdown vg xf text x y w h =
  let cornerRadius = 4.0 in

  C.new_path vg xf;
  C.round_rect vg (x+.1.0) (y+.1.0) (w-.2.0) (h-.2.0) (cornerRadius-.1.0);
  C.fill vg (Paint.linear_gradient x y x (y+.h)
               (Color.gray ~a:0.08 1.0) (Color.gray ~a:0.08 0.0));

  C.new_path vg xf;
  C.round_rect vg (x+.0.5) (y+.0.5) (w-.1.0) (h-.1.0) (cornerRadius-.0.5);
  C.stroke vg (Paint.color (Color.gray ~a:0.1875 0.0)) Outline.default;

  (* nvgFontSize(vg, 20.0f); *)
  (* nvgFontFace(vg, "sans"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,160)); *)
  (* nvgTextAlign(vg,NVG_ALIGN_LEFT|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+h*0.3f,y+h*0.5f,text, NULL); *)

  (* nvgFontSize(vg, h*1.3f); *)
  (* nvgFontFace(vg, "icons"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,64)); *)
  (* nvgTextAlign(vg,NVG_ALIGN_CENTER|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+w-h*0.5f, y+h*0.5f, cpToUTF8(ICON_CHEVRON_RIGHT,icon), NULL); *)
  ()

let draw_label vg xf text x y w h =
  (* nvgFontSize(vg, 18.0f); *)
  (* nvgFontFace(vg, "sans"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,128)); *)

  (* nvgTextAlign(vg,NVG_ALIGN_LEFT|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x,y+h*0.5f,text, NULL); *)
  ()

let draw_editboxbase vg xf x y w h =
  C.new_path vg xf;
  C.round_rect vg (x+.1.0) (y+.1.0) (w-.2.0) (h-.2.0) (4.0-.1.0);
  C.fill vg (Paint.box_gradient (x+.1.0) (y+.1.0+.1.5) (w-.2.0) (h-.2.0) 3.0 4.0
               (Color.gray ~a:0.125 1.0) (Color.gray ~a:0.125 0.125));
  C.new_path vg xf;
  C.round_rect vg (x+.0.5) (y+.0.5) (w-.1.0) (h-.1.0) (4.0-.0.5);
  C.stroke vg (Paint.color (Color.gray ~a:0.1875 0.0)) Outline.default

let draw_editbox vg xf text x y w h =
  draw_editboxbase vg xf x y w h;

  (* nvgFontSize(vg, 20.0f); *)
  (* nvgFontFace(vg, "sans"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,64)); *)
  (* nvgTextAlign(vg,NVG_ALIGN_LEFT|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+h*0.3f,y+h*0.5f,text, NULL); *)
  ()

let draw_editboxnum vg xf text units x y w h =
  draw_editboxbase vg xf x y w h;

  (* uw = nvgTextBounds(vg, 0,0, units, NULL, NULL); *)

  (* nvgFontSize(vg, 18.0f); *)
  (* nvgFontFace(vg, "sans"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,64)); *)
  (* nvgTextAlign(vg,NVG_ALIGN_RIGHT|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+w-h*0.3f,y+h*0.5f,units, NULL); *)

  (* nvgFontSize(vg, 20.0f); *)
  (* nvgFontFace(vg, "sans"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,128)); *)
  (* nvgTextAlign(vg,NVG_ALIGN_RIGHT|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+w-uw-h*0.5f,y+h*0.5f,text, NULL); *)
  ()

let draw_slider vg xf pos x y w h =
  let cy = y +. floor (h*.0.5) in
  let kr = floor (h*.0.25) in

  (* Slot *)
  C.new_path vg xf;
  C.round_rect vg x (cy-.2.) w 4.0 2.0;
  C.fill vg (Paint.box_gradient x (cy-.2.0+.1.0) w 4.0 2.0 2.0
               (Color.gray ~a:0.125 0.0) (Color.gray ~a:0.5 0.0));

  (* Knob Shadow *)
  C.new_path vg xf;
  C.rect vg (x+.floor(pos*.w)-.kr-.5.0) (cy-.kr-.5.0)
    (kr*.2.0+.5.0+.5.0) (kr*.2.0+.5.0+.5.0+.3.0);
  C.circle vg (x+.floor(pos*.w)) cy kr;
  C.set_winding vg `HOLE;
  C.fill vg (Paint.radial_gradient (x+.floor(pos*.w)) (cy+.1.0) (kr-.3.0) (kr+.3.0)
               (Color.gray ~a:0.25 0.0) (Color.gray ~a:0.0 0.0));

  (* Knob *)
  C.new_path vg xf;
  C.circle vg (x+.floor(pos*.w)) cy (kr-.1.0);
  C.fill vg (Paint.color (Color.v_srgbi 40 43 48));
  C.fill vg (Paint.linear_gradient x (cy-.kr) x (cy+.kr)
               (Color.gray ~a:0.0625 1.0) (Color.gray ~a:0.0625 0.0));

  C.new_path vg xf;
  C.circle vg (x+.floor(pos*.w)) cy (kr-.0.5);
  C.stroke vg (Paint.color (Color.gray ~a:0.375 0.0)) Outline.default;

  ()

let draw_checkbox vg xf text x y w h =
  (* nvgFontSize(vg, 18.0f); *)
  (* nvgFontFace(vg, "sans"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,160)); *)

  (* nvgTextAlign(vg,NVG_ALIGN_LEFT|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+28,y+h*0.5f,text, NULL); *)

  C.new_path vg xf;
  C.round_rect vg (x+.1.0) (y+.floor(h/.2.0)-.9.0) 18.0 18.0 3.0;
  C.fill vg (Paint.box_gradient (x+.1.0) (y+.floor(h/.2.0)-.9.0+.1.0)
               18.0 18.0 3.0 3.0
               (Color.gray ~a:0.125 0.0) (Color.gray ~a:0.375 0.0));

  (* nvgFontSize(vg, 40); *)
  (* nvgFontFace(vg, "icons"); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,128)); *)
  (* nvgTextAlign(vg,NVG_ALIGN_CENTER|NVG_ALIGN_MIDDLE); *)
  (* nvgText(vg, x+9+2, y+h*0.5f, cpToUTF8(ICON_CHECK,icon), NULL); *)
  ()

let draw_button vg xf preicon text x y w h col =
  let is_black = Color.a col > 0.0 in
  let cornerRadius = 4.0 in
  C.new_path vg xf;
  C.round_rect vg (x+.1.0) (y+.1.0) (w-.2.0) (h-.2.0) (cornerRadius-.1.0);
  if is_black then (
    C.fill vg (Paint.color col);
  );
  C.fill vg (Paint.linear_gradient x y x (y+.h)
               (Color.gray 1.0 ~a:(if is_black then 0.125 else 0.25))
               (Color.gray 0.0 ~a:(if is_black then 0.125 else 0.25)));
  C.new_path vg xf;
  C.round_rect vg (x+.0.5) (y+.0.5) (w-.1.0) (h-.1.0) (cornerRadius-.0.5);
  C.stroke vg (Paint.color (Color.gray ~a:0.375 0.0)) Outline.default;

  (* float tw = 0, iw = 0; *)
  (* nvgFontSize(vg, 20.0f); *)
  (* nvgFontFace(vg, "sans-bold"); *)
  (* tw = nvgTextBounds(vg, 0,0, text, NULL, NULL); *)
  (* if (preicon != 0) { *)
  (*   nvgFontSize(vg, h*1.3f); *)
  (*   nvgFontFace(vg, "icons"); *)
  (*   iw = nvgTextBounds(vg, 0,0, cpToUTF8(preicon,icon), NULL, NULL); *)
  (*   iw += h*0.15f; *)
  (* } *)

  (* if (preicon != 0) { *)
  (*   nvgFontSize(vg, h*1.3f); *)
  (*   nvgFontFace(vg, "icons"); *)
  (*   nvgFillColor(vg, nvgRGBA(255,255,255,96)); *)
  (*   nvgTextAlign(vg,NVG_ALIGN_LEFT|NVG_ALIGN_MIDDLE); *)
  (*   nvgText(vg, x+w*0.5f-tw*0.5f-iw*0.75f, y+h*0.5f, cpToUTF8(preicon,icon), NULL); *)
  (* } *)

  (* nvgFontSize(vg, 20.0f); *)
  (* nvgFontFace(vg, "sans-bold"); *)
  (* nvgTextAlign(vg,NVG_ALIGN_LEFT|NVG_ALIGN_MIDDLE); *)
  (* nvgFillColor(vg, nvgRGBA(0,0,0,160)); *)
  (* nvgText(vg, x+w*0.5f-tw*0.5f+iw*0.25f,y+h*0.5f-1,text, NULL); *)
  (* nvgFillColor(vg, nvgRGBA(255,255,255,160)); *)
  (* nvgText(vg, x+w*0.5f-tw*0.5f+iw*0.25f,y+h*0.5f,text, NULL); *)
  ()

let image_size image = Wall_tex.width image, Wall_tex.height image
let image_texture image = image

let load_demo_data () =
  Array.init 12 (fun i ->
      let name = Printf.sprintf
          "/Users/def/Sandbox/nanovg/example/images/image%d.jpg" (i+1)
      in
      match Wall_tex.load_image ~alpha:false ~name name with
      | Result.Ok image -> image
      | Result.Error (`Msg msg) ->
        Printf.eprintf "error loading %s: %s\n%!" name msg;
        exit 1
    )

let draw_thumbnails vg xf x y w h images t =
  let cornerRadius = 3.0 and thumb = 60.0 and arry = 30.5 in
  let stackh = float (Array.length images / 2) *. (thumb +. 10.0) +. 10.0 in
  let u = (1.0 +. cos (t*.0.5)) *. 0.5 in
  let u2 = (1.0 -. cos (t*.0.2)) *. 0.5 in

  (* Drop shadow *)
  C.new_path vg xf;
  C.rect vg (x-.10.0) (y-.10.0) (w+.20.0) (h+.30.0);
  C.round_rect vg x y w h cornerRadius;
  C.set_winding vg `HOLE;
  C.fill vg (Paint.box_gradient x (y+.4.0) w h (cornerRadius*.2.0) 20.0
               (Color.gray ~a:0.5 0.0) (Color.gray ~a:0.0 0.0) );

  (* Window *)
  C.new_path vg xf;
  C.round_rect vg x y w h cornerRadius;
  C.move_to vg (x -. 10.0) (y +. arry);
  C.line_to vg (x +. 1.0) (y +. arry -. 11.0);
  C.line_to vg (x +. 1.0) (y +. arry +. 11.0);
  C.fill vg (Paint.color (Color.gray 0.8));

  let frame = Frame.set_scissor x y w h xf Frame.default in
  let xf' = Transform.translate 0.0 (-. (stackh -. h) *. u) xf in
  let dv = 1.0 /. float (Array.length images - 1) in

  Array.iteri (fun i image ->
      let tx = x +. 10.0 +. float (i mod 2) *. (thumb +. 10.0) in
      let ty = y +. 10.0 +. float (i / 2) *. (thumb +. 10.0) in

      let imgw, imgh = image_size image in
      let imgw, imgh = float imgw, float imgh in
      let iw, ih, ix, iy =
        if imgw < imgh then
          let iw = thumb in
          let ih = iw *. imgh /. imgw in
          (iw, ih, 0.0, -.(ih -. thumb) *. 0.5)
        else
          let ih = thumb in
          let iw = ih *. imgw /. imgh in
          (iw, ih, -.(iw -. thumb) *. 0.5, 0.0)
      in
      let v = float i *. dv in
      let a = max 0.0 (min 1.0 ((u2 -. v) /. dv)) in

      if a < 1.0 then
        draw_spinner ~frame vg xf'
          (tx +. thumb /. 2.0) (ty +. thumb /. 2.0) (thumb*.0.25) t;

      C.new_path vg xf';
      C.round_rect vg tx ty thumb thumb 5.0;
      C.fill ~frame vg (Paint.image_pattern
                   (Gg.P2.v (tx+.ix) (ty+.iy)) (Gg.Size2.v iw ih)
                   0.0 a (image_texture image));

      C.new_path vg xf';
      C.rect vg (tx-.5.0) (ty-.5.0) (thumb+.10.0) (thumb+.10.0);
      C.round_rect vg tx ty thumb thumb 6.0;
      C.set_winding vg `HOLE;
      C.fill ~frame vg (Paint.box_gradient (tx-.1.0) ty (thumb+.2.0) (thumb+.2.0) 5.0 3.0
                   (Color.gray ~a:0.5 0.0) (Color.gray ~a:0.0 0.0));

      C.new_path vg xf';
      C.round_rect vg (tx+.0.5) (ty+.0.5) (thumb-.1.0) (thumb-.1.0) (4.0-.0.5);
      C.stroke ~frame vg (Paint.color (Color.gray ~a:0.75 1.0))
        Outline.{default with stroke_width = 1.0};
    ) images;

  (* Hide fades *)
  C.new_path vg xf;
  C.rect vg (x+.4.0) y (w-.8.0) 6.0;
  C.fill vg (Paint.linear_gradient x y x (y+.6.0)
               (Color.gray ~a:1.0 0.8) (Color.gray ~a:0.0 0.8));

  C.new_path vg xf;
  C.rect vg (x+.4.0) (y+.h-.6.0) (w-.8.0) 6.0;
  C.fill vg (Paint.linear_gradient x (y+.h-.6.0) x (y+.6.0)
               (Color.gray ~a:1.0 0.8) (Color.gray ~a:0.0 0.8));

  (* Scroll bar *)
  C.new_path vg xf;
  C.round_rect vg (x+.w-.12.0) (y+.4.0) 8.0 (h-.8.0) 3.0;
  C.fill vg (Paint.box_gradient (x+.w-.12.0+.1.0) (y+.4.0+.1.0) 8.0 (h-.8.0)
               3.0 4.0 (Color.gray ~a:0.125 0.0) (Color.gray ~a:0.375 0.0));

  let scrollh = (h/.stackh) *. (h-.8.0) in
  C.new_path vg xf;
  C.round_rect vg (x+.w-.12.+.1.) (y+.4.+.1. +. (h-.8.-.scrollh)*.u)
    (8.-.2.) (scrollh-.2.) 2.;
  C.fill vg (Paint.box_gradient (x+.w-.12.-.1.) (y+.4.+.(h-.8.-.scrollh)*.u-.1.)
               8. scrollh 3. 4.
               (Color.gray ~a:0.9 1.0) (Color.gray ~a:0.5 1.0))

let images = lazy (load_demo_data ())

let draw_demo vg xf mx my w h t = (
  draw_eyes vg xf (w -. 250.0) 50.0 150.0 100.0 mx my t;
  draw_graph vg xf 0.0 (h /. 2.0) w (h /. 2.0) t;
  draw_colorwheel vg xf (w -. 300.0) (h -. 300.0) 250.0 250.0 t;
  draw_lines vg xf 120.0 (h -. 50.0) 600.0 50.0 t;
  draw_widths vg xf 10.0 50.0 30.0;
  draw_caps vg xf 10.0 300.0 30.0;
  draw_scissor vg xf 50.0 (h-.80.0) t;

  (* Widgets *)
  draw_window vg xf "Widgets `n Stuff" 50.0 50.0 300.0 400.0;
  let x = 60.0 and y = 95.0 in
  draw_searchbox vg xf "Search" x y 280.0 25.0;
  let y = y +. 40.0 in
  draw_dropdown vg xf "Effects" x y 280.0 28.0;
  let popy = y +. 14.0 in
  let y = y +. 45.0 in

  (* Form *)
  draw_label vg xf "login" x y 280.0 20.0;
  let y = y +. 25.0 in
  draw_editbox vg xf "Email" x y 280.0 28.0;
  let y = y +. 35.0 in
  draw_editbox vg xf "Password" x y 280.0 28.0;
  let y = y +. 38.0 in
  draw_checkbox vg xf "Remember me" x y 140.0 28.0;
  draw_button vg xf (-1) "Sign in" (x+.138.0) y 140.0 28.0
    (Color.v 0.0 0.375 0.5 1.0);
  let y = y +. 45.0 in

  (* Slider *)
  draw_label vg xf "Diameter" x y 280.0 20.0;
  let y = y +. 25.0 in
  draw_editboxnum vg xf "123.00" "px" (x+.180.0) y 100.0 28.0;
  draw_slider vg xf 0.4 x y 170.0 28.0;
  let y = y +. 55.0 in

  draw_button vg xf (-1) "Delete" x y 160.0 28.0 (Color.v 0.5 0.0625 0.03125 1.0);
  draw_button vg xf 0 "Cancel" (x+.170.0) y 110.0 28.0 (Color.gray ~a:0.0 0.0);

  draw_thumbnails vg xf 365.0 (popy-.30.0) 160.0 300.0 (Lazy.force images) t;
  ()
)

let render vg t =
  C.new_frame vg;
  let _, (x, y) = Sdl.get_mouse_state () in
  draw_demo vg Transform.identity (float x) (float y) 1000.0 600.0 t;
  C.flush_frame vg (Gg.V2.v 1000.0 600.0)

open Tgles2

let main () =
  Printexc.record_backtrace true;
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    match Sdl.create_window ~w:1000 ~h:600 "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      (*Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;*)
      (*Sdl.gl_set_attribute Sdl.Gl.context_major_version 2;*)
      (*Sdl.gl_set_attribute Sdl.Gl.context_minor_version 1;*)
      ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
      match Sdl.gl_create_context w with
      | Error (`Msg e) -> Sdl.log "Create context error: %s" e; exit 1
      | Ok ctx ->
        let vg = C.create_gl ~antialias:false in
        let t = ref 0.0 in
        for i = 0 to 1000 do
          Sdl.pump_events ();
          Unix.sleepf 0.020;
          t := !t +. 0.050;
          Gl.viewport 0 0 1000 600;
          Gl.clear_color 0.3 0.3 0.32 1.0;
          Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
          Gl.enable Gl.blend;
          Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha;
          Gl.enable Gl.cull_face_enum;
          Gl.disable Gl.depth_test;
          render vg !t;
          Sdl.gl_swap_window w;
        done;
        Sdl.gl_delete_context ctx;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()
