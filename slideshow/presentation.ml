open Wall
open Wall_text
open Pres_state
;;

let default_font ?(size=1.0) () =
  Font.make ~size:(64.0 *. size) font_sans
    ~placement:`Subpixel

let title_banner = Path.make (fun ctx ->
    Path.rect ctx ~x:0.0 ~y:0.0 ~w:1024.0 ~h:128.0
  )

let b2 x y w h = Gg.Box2.v (Gg.P2.v x y) (Gg.Size2.v w h)
let body_box = b2 0.0 128.0 1024.0 640.0

let light_blue = Color.lerp_rgba 0.5 Color.white Color.blue

let title =
  let color = Paint.rgba 0.0 0.0 0.0 1.0 in
  let title_fg = Font.make ~size:96.0 font_sans in
  let title_bg = Font.make ~blur:6.0 ~size:96.0 font_sans in
  fun text content->
    [ Image.paint Paint.white (Image.fill mediabox);
      Image.paint background (Image.fill title_banner);
      Image.paint color (
        Image.seq [
          simple_text title_bg text
            ~x:514.0 ~y:18.0 ~halign:`CENTER ~valign:`TOP;
          simple_text title_fg text
            ~x:512.0 ~y:16.0 ~halign:`CENTER ~valign:`TOP;
        ]
      );
      Image.scissor body_box (Image.seq content)
    ]

let draw_arrow ~x ~y ~size color =
  Image.paint (Paint.color color)
    (Image.fill_path @@ fun t ->
     Path.move_to t x y;
     Path.line_to t (x-.size) (y+.size);
     Path.line_to t (x-.size) (y-.size);
     Path.close t;
    )

let text ?(halign=`LEFT) ?size ~x ~y str =
  simple_text ~x ~y ~halign ~valign:`BASELINE (default_font ?size ()) str

let text_arrow ~x ~y str =
  Image.seq [
    draw_arrow ~x ~y ~size:16.0 Color.blue;
    text ~x:(x +. 20.0) ~y:(y +. 14.0) str
  ]

let make_outlines a_title steps step =
  let render_step i (step', title) =
    let text = text_arrow ~x:250.0 ~y:(250.0 +. 100.0 *. float i) title in
    if step = step' then
      text
    else
      Image.alpha 0.5 text
  in
  let steps = List.mapi render_step steps in
  title a_title steps

let code fmt =
  let size = 36.0 in
  let interline = size +. 4.0 in
  let font = Font.make ~size font_mono in
  Printf.ksprintf
    (fun source ->
       let rec lines i =
         match String.index_from source i '\n' with
         | j -> String.sub source i (j - i) :: lines (j + 1)
         | exception Not_found -> [String.sub source i (String.length source - i)]
       in
       let lines = lines 0 in
       let h = 768.0 -. interline *. float (List.length lines) in
       Image.seq (
         List.mapi (fun i line ->
             Wall_text.simple_text font line
               ~halign:`LEFT ~valign:`BASELINE
               ~x:10.0 ~y:(h +. float i *. interline)
           ) lines
       ))
    fmt

let pf () f =
  if f < 0.0 then
    Printf.sprintf "(%.02f)" f
  else
    Printf.sprintf "%.02f" f

let outline =
  make_outlines "Outline"
  [
    `Problem_solved , "The problem solved";
    `Model          , "Model";
    `API            , "API";
    `Conclusion     , "Conclusion";
  ]

let api_outline =
  make_outlines "API"
    [
      `Path          , "Path";
      `Shape         , "Shape";
      `Paint         , "Paint";
      `Transformation , "Transformation";
      `Composition   , "Composition";
    ]

let shape_slide =
  let square = Path.make (fun ctx ->
      Path.rect ctx (-150.0) (-50.0) 100.0 100.0
    ) in
  let circle = Path.make (fun ctx ->
      Path.circle ctx 100.0 0.0 100.0
    ) in
  title "Shapes" [
    Image.transform (Transform.translation 512.0 444.0)
      (Image.seq [ Image.fill square; Image.fill circle ])
  ]
;;

Slideshow.set_slides Slideshow.window [
  (fun _ -> title "The Wall library"
      [
        text ~halign:`CENTER ~x:512.0 ~y:250.0 "Frédéric Bour <def@fb.com>";
        text ~halign:`CENTER ~x:512.0 ~y:350.0 "IRILL, Paris";
        text ~halign:`CENTER ~x:512.0 ~y:450.0 "Thursday, July 5";
        text_arrow ~x:152.0 ~y:600.0 "Graphics";
        text_arrow ~x:412.0 ~y:600.0 "In OCaml";
        text_arrow ~x:672.0 ~y:600.0 "Without Pain";
      ]
  );
  (fun _ -> outline `Problem_solved);
  (fun _ -> title "Key points"
      [
        text ~x:80.0 ~y:280.0 "1) Declarative vector graphics";
        text ~x:80.0 ~y:450.0 "2) Rendered with OpenGL";
        text ~x:80.0 ~y:620.0 "3) Independent from a windowing system";
      ]
  );
  (fun _ -> title "Tradeoffs"
      [
        text_arrow ~x:100.0 ~y:200.0 "Made for user interfaces";
        text_arrow ~x:100.0 ~y:300.0 "Goals:";
        text ~x:140.0 ~y:380.0 "(1) High output quality";
        text ~x:140.0 ~y:440.0 "(2) Performance";
        text ~x:140.0 ~y:500.0 "(3) Simplicity";
        text_arrow ~x:100.0 ~y:580.0 "Non-goal: expressivity";
      ]
  );
  (fun _ -> outline `Model);
  (fun _ -> shape_slide);
  (fun st ->
     let paint_slide =
       let rect = Path.make (fun ctx ->
           Path.rect ctx 0.0 128.0 1024.0 640.0
         ) in
       title "Paint" [
         Image.paint (Paint.color (Color.v_srgb 1.0 1.0 0.0))
           (Image.fill rect)
       ]
     in
     if st.time > 0.25
     then paint_slide
     else
       let progress = st.time /. 0.25 in
       [
         Image.transform
           (Transform.translation (-. progress *. 1024.0) 0.0)
           (Image.seq shape_slide);
         Image.transform
           (Transform.translation ((1.0 -. progress) *. 1024.0) 0.0)
           (Image.seq paint_slide);
       ]
     );
  (fun st ->
     let circle = Path.make (fun ctx ->
         Path.circle ctx 0.0 0.0 100.0
       ) in
     let paint = Paint.color (Color.v_srgb 1.0 1.0 0.0) in
     title "Painted shapes" [
       Image.transform (Transform.translation 512.0 444.0)
         (Image.transform
            (let f = max (10.0 *. (1.0 -. st.time)) 1.0 in Transform.scale f f)
            (Image.paint paint (Image.fill circle)))
     ]);
  (fun st ->
     let circle = Path.make (fun ctx ->
         Path.circle ctx 0.0 0.0 100.0
       ) in
     let paint = Paint.color (Color.v_srgb 1.0 1.0 0.0) in
     title "Transform" [
       Image.transform (Transform.translation 512.0 444.0)
         (Image.transform
            (let f = (1.0 +. min st.time 1.0) in
             Transform.scale (1.0 +. f /. 4.0) f)
            (Image.paint paint (Image.fill circle)))
     ]);
  (fun st ->
     let outline = Outline.make ~cap:`ROUND ~width:10.0 () in
     let circle = Path.make (fun ctx ->
         Path.circle ctx 0.0 0.0 100.0
       ) in
     let eye =
       let contour = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 30.0
         ) in
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 10.0
         ) in
       Image.seq [
         Image.paint Paint.white (Image.fill contour);
         Image.stroke outline contour;
         Image.fill dot;
       ]
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       Image.stroke outline path
     in
     let base = Color.v_srgb 1.0 1.0 0.0 in
     let paint =
       Paint.linear_gradient (-100.0) 0.0 350.0 300.0 base Color.black
     in
     title "Repeat!" [
       Image.transform (Transform.translation 512.0 444.0) (Image.seq [
           Image.transform
             (Transform.scale 1.5 2.0)
             (Image.paint paint (Image.fill circle));
           Image.transform (Transform.translation (-60.0) (-90.0)) eye;
           Image.transform (Transform.translation (60.0) (-90.0)) eye;
           Image.transform (Transform.translation 0.0 10.0) smile;
         ])
     ]);
  (fun _ -> outline `API);
  (fun _ -> api_outline `Path);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "Path.make (fun ctx ->\n\
            \  \n\
            \  \n\
            \  \n\
            \  \n\
             )"
     ]);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "Path.make (fun ctx ->\n\
            \  Path.move_to ctx 0.0 0.0;\n\
            \  \n\
            \  \n\
            \  \n\
             )"
     ]);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
         Path.line_to ctx 50.0 0.0;
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "Path.make (fun ctx ->\n\
            \  Path.move_to ctx 0. 0.;\n\
            \  Path.line_to ctx 5. 0.;\n\
            \  \n\
            \  \n\
             )"
     ]);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
         Path.line_to ctx 50.0 0.0;
         Path.bezier_to ctx 100.0 (-30.0) 100.0 (-90.0) 50.0 (-60.0);
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "Path.make (fun ctx ->\n\
            \  Path.move_to ctx 0. 0.;\n\
            \  Path.line_to ctx 5. 0.;\n\
            \  Path.bezier_to ctx 10.0 (-3.) 10.0 (-9.) 5.0 (-6.);\n\
            \  \n\
             )"
     ]);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
         Path.line_to ctx 50.0 0.0;
         Path.bezier_to ctx 100.0 (-30.0) 100.0 (-90.0) 50.0 (-60.0);
         Path.bezier_to ctx 60.0 (-120.0) (-20.0) (-120.0) 0.0 0.0;
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "Path.make (fun ctx ->\n\
            \  Path.move_to ctx 0.0 0.0;\n\
            \  Path.line_to ctx 50.0 0.0;\n\
            \  Path.bezier_to ctx 10.0 (-3.) 10.0 (-9.) 5.0 (-6.);\n\
            \  Path.bezier_to ctx 6.0 (-12.0) (-2.0) (-12.0) 0. 0.;\n\
             )"
     ]);
  (fun _ -> api_outline `Shape);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
         Path.line_to ctx 50.0 0.0;
         Path.bezier_to ctx 100.0 (-30.0) 100.0 (-90.0) 50.0 (-60.0);
         Path.bezier_to ctx 60.0 (-120.0) (-20.0) (-120.0) 0.0 0.0;
       )
     in
     title "Filling path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.fill p);
       code "Image.fill path";
     ]);
  (fun st ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
         Path.line_to ctx 50.0 0.0;
         Path.bezier_to ctx 100.0 (-30.0) 100.0 (-90.0) 50.0 (-60.0);
         Path.bezier_to ctx 60.0 (-120.0) (-20.0) (-120.0) 0.0 0.0;
       )
     in
     let width = (sin st.time *. sin st.time) *. 10.0 in
     title "Stroking path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke (Outline.make ~width ()) p);
       code "Image.stroke (Outline.make ~width:%a ()) path" pf width;
     ]);
  (fun _ -> api_outline `Paint);
  (fun _ ->
     title "Paint: color" [
       Image.paint (Paint.color Color.red)
         (Image.fill mediabox);
       code "Paint.color red"
     ]);
  (fun st ->
     let t = st.time /. 4.0 in
     let s = sin (t *. 2.0) and c = cos (t *. 3.0) in
     let sx = 24.0 +. abs_float s *. 200.0 in
     let ex = 1000.0 -. abs_float c *. 200.0 in
     title "Paint: linear" [
       Image.paint (Paint.linear_gradient sx 0.0 ex 0.0
                      Color.red light_blue)
         (Image.fill mediabox);
       code "Paint.linear_gradient %a 0.0 %a 0.0" pf sx pf ex
     ]);
  (fun st ->
     let t = st.time /. 4.0 in
     let s = sin (t *. 2.0) and c = cos (t *. 3.0) in
     let r = abs_float (s *. 100.0) in
     let f = abs_float (c *. 50.0) in
     title "Paint: box" [
       Image.paint (Paint.box_gradient 160.0 228.0 704.0 360.0 r f
                      Color.red light_blue)
         (Image.fill mediabox);
       code "Paint.box_gradient \n\
            \  ~x ~y ~w ~h ~r:%a ~f:%a red light_blue" pf r pf f
     ]);
  (fun st ->
     let t = st.time /. 4.0 in
     let s = sin (t *. 2.0) and c = cos (t *. 3.0) in
     let inner = s *. s *. 200.0 in
     let outer = inner +. c *. c *. 100.0 in
     title "Paint: radial" [
       Image.paint (Paint.radial_gradient 512.0 444.0 inner outer
                      Color.red light_blue)
         (Image.fill mediabox);
       code "Paint.radial_gradient \n\
            \  ~cx ~cy ~inner:%a ~outer:%a" pf inner pf outer;
     ]);
  (fun _ -> api_outline `Transformation);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let sx = sin (st.time *. 2.0) *. 200.0 in
     let sy = cos (st.time *. 3.0) *. 200.0 in
     title "Transformation: translation" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform
            (Transform.translation sx sy)
            (Image.fill rect));
       code "Image.transform (translation %a %a) rect" pf sx pf sy;
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let a = st.time *. 2.0 in
     title "Transformation: rotation" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform
            (Transform.rotation a)
            (Image.fill rect));
       code "Image.transform (rotation %a) rect" pf a;
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let sx = 1.0 +. sin (st.time *. 2.0) *. 2.0 in
     let sy = 1.0 +. cos (st.time *. 3.0) *. 4.0 in
     title "Transformation: scaling" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform (Transform.scale sx sy) (Image.fill rect));
       code "Image.transform (scale %a %a) rect" pf sx pf sy;
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let sx = sin (st.time *. 2.0) in
     let sy = cos (st.time *. 3.0) in
     title "Transformation: skewing" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform (Transform.skew sx sy) (Image.fill rect));
       code "Image.transform (skew %a %a) rect" pf sx pf sy;
     ]);
  (fun _ -> api_outline `Composition);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-120.0) (-120.0) 240.0 240.0)
     in
     title "Composition: paint" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.paint
            (Paint.linear_gradient (-120.0) (-120.0) 120.0 120.0
                      Color.red light_blue)
            (Image.fill rect));
       code "Image.paint linear_gradient square";
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-120.0) (-120.0) 240.0 240.0)
     in
     let circle =
       Path.make (fun ctx -> Path.circle ctx 0.0 0.0 120.0)
     in
     let t = st.time *. 3.0 in
     let y = 200.0 *. sin t in
     let a = t /. 10.0 in
     title "Composition: superposition" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.seq [
             Image.paint
               (Paint.linear_gradient (-120.0) (-120.0) 120.0 120.0
                  Color.red light_blue)
               (Image.fill rect);
             Image.transform
               (Transform.translate 0.0 y (Transform.rotation a))
               (Image.paint
                  (Paint.radial_gradient 0.0 0.0 20.0 180.0
                     Color.red light_blue)
                  (Image.fill circle));
           ]);
       code
         "Image.superpose square\n\
         \  (moving_circle ~offset:%a ~angle:%a)"
         pf y pf a
     ]);
  (fun st ->
     let circle =
       Path.make (fun ctx -> Path.circle ctx 0.0 0.0 120.0)
     in
     let t = st.time *. 3.0 in
     let y = 200.0 *. sin t in
     let a = t /. 10.0 in
     title "Composition: scissor" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.seq [
             Image.scissor (b2 (-120.0) (-120.0) 240.0 240.0)
               (Image.transform
                  (Transform.translate 0.0 y
                     (Transform.rotation a))
                  (Image.paint
                     (Paint.radial_gradient 0.0 0.0 20.0 180.0
                        Color.red light_blue)
                     (Image.fill circle)))
           ]);
       code
         "Image.scissor box\n\
         \  (moving_circle ~offset:%a ~angle:%a)"
         pf y pf a
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-120.0) (-120.0) 240.0 240.0)
     in
     let circle =
       Path.make (fun ctx -> Path.circle ctx 0.0 0.0 120.0)
     in
     let v_alpha = (0.5 +. sin st.time /. 2.0) in
     title "Composition: alpha" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.seq [
             Image.paint
               (Paint.linear_gradient (-120.0) (-120.0) 120.0 120.0
                  Color.red light_blue)
               (Image.fill rect);
             Image.alpha v_alpha
               (Image.paint
                  (Paint.radial_gradient 0.0 0.0 20.0 180.0
                     Color.red light_blue)
                  (Image.fill circle));
           ]);
       code "Image.superpose square (Image.alpha %a circle)" pf v_alpha
     ]);
  (fun _ -> outline `Conclusion);
  (fun _ -> title "Future work"
      [
        text_arrow ~x:100.0 ~y:200.0 "FOO";
        text_arrow ~x:100.0 ~y:300.0 "BAR";
        text_arrow ~x:100.0 ~y:400.0 "BAZ";
      ]
  );
  (fun _ -> title "Acknowledgements"
      [
        text ~x:100.0 ~y:250.0 "Thanks to Pierre Chambart,";
        text ~x:110.0 ~y:350.0 "Sébastien Mondet and Jordan Walke.";
        text ~x:100.0 ~y:550.0 "Thanks to Mikko Mononen for NanoVG,";
        text ~x:110.0 ~y:650.0 "the original engine behind Wall.";
      ]
  );
  (fun _ -> title "Acknowledgements"
      [
        text ~x:100.0 ~y:450.0 "And one last thing...";
        text ~x:100.0 ~y:650.0 "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn.";
      ]
  )]
;;
