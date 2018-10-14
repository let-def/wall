[@@@ocaml.warning "-6"]
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

let text ?(halign=`LEFT) ?size ~x ~y str =
  simple_text ~x ~y ~halign ~valign:`BASELINE (default_font ?size ()) str

let slides = [
  (fun _ ->
     let circle = Path.make (fun ctx ->
         Path.circle ctx 0.0 0.0 100.0
       ) in
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let base = Color.v 0.95 0.95 0.0 1.0 in
     let node ?connect x y str =
       Image.seq [
         text ~halign:`CENTER ~x ~y ~size:0.6 str;
         Image.alpha 0.5 (
           match connect with
           | None -> Image.empty
           | Some (ox, oy) ->
             let oy = oy -. 30.0 in
             let y = y -. 40.0 in
             let dx = x -. ox in
             let dy = y -. oy in
             let x = ox +. dx *. 0.55 in
             let y = oy +. dy *. 0.55 in
             Image.stroke_path (Outline.make ~cap:`ROUND ~width:3.0 ())
               (fun p ->
                  Path.move_to p ~x:ox ~y:oy;
                  Path.line_to p ~x ~y;
               )
         )
       ]
     in
     let paint = Paint.color base in
     title "Image representation" [
       Image.transform (Transform.translation 812.0 384.0) (Image.seq [
           Image.transform
             (Transform.scale 1.5 1.5)
             (Image.paint paint (Image.fill circle));
           Image.transform (Transform.translation (-50.0) (-50.0)) dot;
           Image.transform (Transform.translation (50.0) (-50.0)) dot;
           Image.transform (Transform.translation 0.0 40.0) smile;
         ]);
       node 300.0 600.0 "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:(300.0,600.0);
       node 100.0 400.0 "Circle" ~connect:(100.0,500.0);
       node 450.0 500.0 "paint(black)" ~connect:(300.0,600.0);
       node 280.0 400.0 "transform" ~connect:(450.0,500.0);
       node 280.0 300.0 "Circle" ~connect:(280.0,400.0);
       node 430.0 400.0 "transform" ~connect:(450.0,500.0);
       node 430.0 300.0 "Circle" ~connect:(430.0,400.0);
       node 580.0 400.0 "transform" ~connect:(450.0,500.0);
       node 580.0 300.0 "Smile" ~connect:(580.0,400.0);
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 240.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 430.0 240.0))
             dot;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 280.0 240.0))
             dot;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 100.0 340.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let circle = Path.make (fun ctx ->
         Path.circle ctx 0.0 0.0 100.0
       ) in
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let base = Color.v 0.95 0.95 0.0 1.0 in
     let node ?(connect=[]) x y str =
       Image.seq [
         text ~halign:`CENTER ~x ~y ~size:0.6 str;
         Image.alpha 0.5 (
           Image.seq (List.map (fun (ox, oy) ->
               let oy = oy -. 30.0 in
               let y = y -. 40.0 in
               let dx = x -. ox in
               let dy = y -. oy in
               let ratio =
                 let dy =abs_float dy in
                 (dy -. 50.0) /. dy
               in
               let x = ox +. dx *. ratio in
               let y = oy +. dy *. ratio in
               Image.stroke_path
                 (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
                     Path.move_to p ~x:ox ~y:oy;
                     Path.line_to p ~x ~y;
                   )
             ) connect)
         )
       ]
     in
     let paint = Paint.color base in
     title "With sharing" [
       Image.transform (Transform.translation 812.0 384.0) (Image.seq [
           Image.transform
             (Transform.scale 1.5 1.5)
             (Image.paint paint (Image.fill circle));
           Image.transform (Transform.translation (-50.0) (-50.0)) dot;
           Image.transform (Transform.translation (50.0) (-50.0)) dot;
           Image.transform (Transform.translation 0.0 40.0) smile;
         ]);
       node 300.0 600.0 "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:[300.0,600.0];
       node 100.0 400.0 "Primitive" ~connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)" ~connect:[300.0,600.0];
       node 280.0 400.0 "transform" ~connect:[450.0,500.0];
       node 280.0 300.0 "Primitive" ~connect:[280.0,400.0];
       node 430.0 400.0 "transform" ~connect:[450.0,500.0];
       node 430.0 300.0 "Primitive" ~connect:[430.0,400.0];
       node 580.0 400.0 "transform" ~connect:[450.0,500.0];
       node 580.0 300.0 "Primitive" ~connect:[580.0,400.0];
       node 330.0 200.0 "Circle" ~connect:[
         100.0,400.0;
         280.0,300.0;
         430.0,300.0;
       ];
       node 580.0 200.0 "Smile" ~connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let node ?(connect=[]) x y str =
       Image.seq [
         text ~halign:`CENTER ~x ~y ~size:0.6 str;
         Image.alpha 0.5 (
           Image.seq (List.map (fun (ox, oy) ->
               let oy = oy -. 30.0 in
               let y = y -. 40.0 in
               let dx = x -. ox in
               let dy = y -. oy in
               let ratio =
                 let dy =abs_float dy in
                 (dy -. 50.0) /. dy
               in
               let x = ox +. dx *. ratio in
               let y = oy +. dy *. ratio in
               Image.stroke_path
                 (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
                     Path.move_to p ~x:ox ~y:oy;
                     Path.line_to p ~x ~y;
                   )
             ) connect)
         )
       ]
     in
     title "Allocating GPU memory" [
       text ~size:0.8 ~x:700.0 ~y:180.0 "GPU memory";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 200.0 300.0 300.0);
       text ~size:0.8 ~x:700.0 ~y:550.0 "GPU command";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 570.0 300.0 60.0);
       (* Nodes *)
       node 300.0 600.0 "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:[300.0,600.0];
       node 100.0 400.0 "Primitive" ~connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)" ~connect:[300.0,600.0];
       node 280.0 400.0 "transform" ~connect:[450.0,500.0];
       node 280.0 300.0 "Primitive" ~connect:[280.0,400.0];
       node 430.0 400.0 "transform" ~connect:[450.0,500.0];
       node 430.0 300.0 "Primitive" ~connect:[430.0,400.0];
       node 580.0 400.0 "transform" ~connect:[450.0,500.0];
       node 580.0 300.0 "Primitive" ~connect:[580.0,400.0];
       node 330.0 200.0 "Circle" ~connect:[
         100.0,400.0;
         280.0,300.0;
         430.0,300.0;
       ];
       node 580.0 200.0 "Smile" ~connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let node ?(connect=[]) x y str =
       Image.seq [
         text ~halign:`CENTER ~x ~y ~size:0.6 str;
         Image.alpha 0.5 (
           Image.seq (List.map (fun (ox, oy) ->
               let oy = oy -. 30.0 in
               let y = y -. 40.0 in
               let dx = x -. ox in
               let dy = y -. oy in
               let ratio =
                 let dy =abs_float dy in
                 (dy -. 50.0) /. dy
               in
               let x = ox +. dx *. ratio in
               let y = oy +. dy *. ratio in
               Image.stroke_path
                 (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
                     Path.move_to p ~x:ox ~y:oy;
                     Path.line_to p ~x ~y;
                   )
             ) connect)
         )
       ]
     in
     title "Allocating GPU memory" [
       text ~size:0.8 ~x:700.0 ~y:180.0 "GPU memory";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 200.0 300.0 300.0);

       Image.paint (Paint.color Color.red)
         (Image.fill_path (fun p -> Path.rect p 700.0 200.0 300.0 60.0));
       text ~size:0.8 ~x:720.0 ~y:245.0 "Circle";

       text ~size:0.8 ~x:700.0 ~y:550.0 "GPU command";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 570.0 300.0 60.0);
       (* Nodes *)
       node 300.0 600.0 "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:[300.0,600.0];
       node 100.0 400.0 "Primitive" ~connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)" ~connect:[300.0,600.0];
       node 280.0 400.0 "transform" ~connect:[450.0,500.0];
       node 280.0 300.0 "Primitive" ~connect:[280.0,400.0];
       node 430.0 400.0 "transform" ~connect:[450.0,500.0];
       node 430.0 300.0 "Primitive" ~connect:[430.0,400.0];
       node 580.0 400.0 "transform" ~connect:[450.0,500.0];
       node 580.0 300.0 "Primitive" ~connect:[580.0,400.0];
       Image.paint (Paint.color Color.red) @@
       node 330.0 200.0 "Circle" ~connect:[
         100.0,400.0;
         280.0,300.0;
         430.0,300.0;
       ];
       node 580.0 200.0 "Smile" ~connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let node ?(connect=[]) x y str =
       Image.seq [
         text ~halign:`CENTER ~x ~y ~size:0.6 str;
         Image.alpha 0.5 (
           Image.seq (List.map (fun (ox, oy) ->
               let oy = oy -. 30.0 in
               let y = y -. 40.0 in
               let dx = x -. ox in
               let dy = y -. oy in
               let ratio =
                 let dy =abs_float dy in
                 (dy -. 50.0) /. dy
               in
               let x = ox +. dx *. ratio in
               let y = oy +. dy *. ratio in
               Image.stroke_path
                 (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
                     Path.move_to p ~x:ox ~y:oy;
                     Path.line_to p ~x ~y;
                   )
             ) connect)
         )
       ]
     in
     title "Allocating GPU memory" [
       text ~size:0.8 ~x:700.0 ~y:180.0 "GPU memory";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 200.0 300.0 300.0);

       Image.paint (Paint.color Color.red)
         (Image.fill_path (fun p -> Path.rect p 700.0 200.0 300.0 60.0));
       text ~size:0.8 ~x:720.0 ~y:245.0 "Circle";

       Image.paint (Paint.color Color.green)
         (Image.fill_path (fun p -> Path.rect p 700.0 260.0 300.0 60.0));
       text ~size:0.8 ~x:720.0 ~y:305.0 "Smile";

       text ~size:0.8 ~x:700.0 ~y:550.0 "GPU command";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 570.0 300.0 60.0);
       (* Nodes *)
       node 300.0 600.0 "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:[300.0,600.0];
       node 100.0 400.0 "Primitive" ~connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)" ~connect:[300.0,600.0];
       node 280.0 400.0 "transform" ~connect:[450.0,500.0];
       node 280.0 300.0 "Primitive" ~connect:[280.0,400.0];
       node 430.0 400.0 "transform" ~connect:[450.0,500.0];
       node 430.0 300.0 "Primitive" ~connect:[430.0,400.0];
       node 580.0 400.0 "transform" ~connect:[450.0,500.0];
       node 580.0 300.0 "Primitive" ~connect:[580.0,400.0];
       node 330.0 200.0 "Circle" ~connect:[
         100.0,400.0;
         280.0,300.0;
         430.0,300.0;
       ];
       Image.paint (Paint.color (Color.lerp_rgba 0.5 Color.black Color.green)) @@
       node 580.0 200.0 "Smile" ~connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let node ?(connect=[]) x y str =
       Image.seq [
         text ~halign:`CENTER ~x ~y ~size:0.6 str;
         Image.alpha 0.5 (
           Image.seq (List.map (fun (ox, oy) ->
               let oy = oy -. 30.0 in
               let y = y -. 40.0 in
               let dx = x -. ox in
               let dy = y -. oy in
               let ratio =
                 let dy =abs_float dy in
                 (dy -. 50.0) /. dy
               in
               let x = ox +. dx *. ratio in
               let y = oy +. dy *. ratio in
               Image.stroke_path
                 (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
                     Path.move_to p ~x:ox ~y:oy;
                     Path.line_to p ~x ~y;
                   )
             ) connect)
         )
       ]
     in
     title "Traversing tree" [
       text ~size:0.8 ~x:700.0 ~y:180.0 "GPU memory";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 200.0 300.0 300.0);

       Image.alpha 0.5 (Image.seq [
           Image.paint (Paint.color Color.red)
             (Image.fill_path (fun p -> Path.rect p 700.0 200.0 300.0 60.0));
           text ~size:0.8 ~x:720.0 ~y:245.0 "Circle";
           Image.paint (Paint.color Color.green)
             (Image.fill_path (fun p -> Path.rect p 700.0 260.0 300.0 60.0));
           text ~size:0.8 ~x:720.0 ~y:305.0 "Smile";
         ]);

       text ~size:0.8 ~x:700.0 ~y:550.0 "GPU command";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 570.0 300.0 60.0);
       (* Nodes *)
       node 300.0 600.0 "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:[300.0,600.0];
       node 100.0 400.0 "Primitive" ~connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)" ~connect:[300.0,600.0];
       node 280.0 400.0 "transform" ~connect:[450.0,500.0];
       node 280.0 300.0 "Primitive" ~connect:[280.0,400.0];
       node 430.0 400.0 "transform" ~connect:[450.0,500.0];
       node 430.0 300.0 "Primitive" ~connect:[430.0,400.0];
       node 580.0 400.0 "transform" ~connect:[450.0,500.0];
       node 580.0 300.0 "Primitive" ~connect:[580.0,400.0];
       node 330.0 200.0 "Circle" ~connect:[
         100.0,400.0;
         280.0,300.0;
         430.0,300.0;
       ];
       node 580.0 200.0 "Smile" ~connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let node ?color ?(color_connect=[]) ?(connect=[]) x y str =
       let line (ox, oy) =
         let oy = oy -. 30.0 in
         let y = y -. 40.0 in
         let dx = x -. ox in
         let dy = y -. oy in
         let ratio =
           let dy =abs_float dy in
           (dy -. 50.0) /. dy
         in
         let x = ox +. dx *. ratio in
         let y = oy +. dy *. ratio in
         Image.stroke_path
           (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
               Path.move_to p ~x:ox ~y:oy;
               Path.line_to p ~x ~y;
             )
       in
       match color with
       | None ->
         Image.seq [
           text ~halign:`CENTER ~x ~y ~size:0.6 str;
           Image.alpha 0.5 (
             Image.seq (List.map line (connect @ color_connect))
           )
         ]
       | Some color ->
         let paint = Paint.color color in
         Image.seq [
           Image.paint paint (text ~halign:`CENTER ~x ~y ~size:0.6 str);
           Image.alpha 0.5 (Image.seq [
               Image.seq (List.map line connect);
               Image.paint paint (Image.seq (List.map line color_connect))
             ]
             )
         ]
     in
     title "Traversing tree" [
       text ~size:0.8 ~x:700.0 ~y:180.0 "GPU memory";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 200.0 300.0 300.0);

       Image.paint (Paint.color Color.red)
         (Image.fill_path (fun p -> Path.rect p 700.0 200.0 300.0 60.0));
       text ~size:0.8 ~x:720.0 ~y:245.0 "Circle";
       Image.alpha 0.5 (Image.seq [
           Image.paint (Paint.color Color.green)
             (Image.fill_path (fun p -> Path.rect p 700.0 260.0 300.0 60.0));
           text ~size:0.8 ~x:720.0 ~y:305.0 "Smile";
         ]);

       text ~size:0.8 ~x:700.0 ~y:550.0 "GPU command";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 570.0 300.0 60.0);
       text ~size:0.6 ~x:720.0 ~y:610.0 "fill(circle,yellow,xf0)";
       (* Nodes *)
       node 300.0 600.0 ~color:Color.red "superpose";
       node 100.0 500.0 "paint(yellow)"
         ~color:Color.red
         ~color_connect:[300.0,600.0];
       node 100.0 400.0 "Primitive"
         ~color:Color.red
         ~color_connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)" ~connect:[300.0,600.0];
       node 280.0 400.0 "transform" ~connect:[450.0,500.0];
       node 280.0 300.0 "Primitive" ~connect:[280.0,400.0];
       node 430.0 400.0 "transform" ~connect:[450.0,500.0];
       node 430.0 300.0 "Primitive" ~connect:[430.0,400.0];
       node 580.0 400.0 "transform" ~connect:[450.0,500.0];
       node 580.0 300.0 "Primitive" ~connect:[580.0,400.0];
       node 330.0 200.0 "Circle"
         ~color:Color.red
         ~color_connect:[100.0,400.0]
         ~connect:[280.0,300.0; 430.0,300.0];
       node 580.0 200.0 "Smile" ~connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let node ?color ?(color_connect=[]) ?(connect=[]) x y str =
       let line (ox, oy) =
         let oy = oy -. 30.0 in
         let y = y -. 40.0 in
         let dx = x -. ox in
         let dy = y -. oy in
         let ratio =
           let dy =abs_float dy in
           (dy -. 50.0) /. dy
         in
         let x = ox +. dx *. ratio in
         let y = oy +. dy *. ratio in
         Image.stroke_path
           (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
               Path.move_to p ~x:ox ~y:oy;
               Path.line_to p ~x ~y;
             )
       in
       match color with
       | None ->
         Image.seq [
           text ~halign:`CENTER ~x ~y ~size:0.6 str;
           Image.alpha 0.5 (
             Image.seq (List.map line (connect @ color_connect))
           )
         ]
       | Some color ->
         let paint = Paint.color color in
         Image.seq [
           Image.paint paint (text ~halign:`CENTER ~x ~y ~size:0.6 str);
           Image.alpha 0.5 (Image.seq [
               Image.seq (List.map line connect);
               Image.paint paint (Image.seq (List.map line color_connect))
             ]
             )
         ]
     in
     title "Traversing tree" [
       text ~size:0.8 ~x:700.0 ~y:180.0 "GPU memory";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 200.0 300.0 300.0);

       Image.paint (Paint.color Color.red)
         (Image.fill_path (fun p -> Path.rect p 700.0 200.0 300.0 60.0));
       text ~size:0.8 ~x:720.0 ~y:245.0 "Circle";
       Image.alpha 0.5 (Image.seq [
           Image.paint (Paint.color Color.green)
             (Image.fill_path (fun p -> Path.rect p 700.0 260.0 300.0 60.0));
           text ~size:0.8 ~x:720.0 ~y:305.0 "Smile";
         ]);

       text ~size:0.8 ~x:700.0 ~y:550.0 "GPU command";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 570.0 300.0 60.0);
       text ~size:0.6 ~x:720.0 ~y:610.0 "fill(circle,black,xf1)";
       (* Nodes *)
       node 300.0 600.0 ~color:Color.red "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:[300.0,600.0];
       node 100.0 400.0 "Primitive" ~connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)"
         ~color:Color.red
         ~color_connect:[300.0,600.0];
       node 280.0 400.0 "transform"
         ~color:Color.red
         ~color_connect:[450.0,500.0];
       node 280.0 300.0 "Primitive"
         ~color:Color.red
         ~color_connect:[280.0,400.0];
       node 430.0 400.0 "transform" ~connect:[450.0,500.0];
       node 430.0 300.0 "Primitive" ~connect:[430.0,400.0];
       node 580.0 400.0 "transform" ~connect:[450.0,500.0];
       node 580.0 300.0 "Primitive" ~connect:[580.0,400.0];
       node 330.0 200.0 "Circle"
         ~color:Color.red
         ~color_connect:[280.0,300.0;]
         ~connect:[100.0,400.0;430.0,300.0];
       node 580.0 200.0 "Smile" ~connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let node ?color ?(color_connect=[]) ?(connect=[]) x y str =
       let line (ox, oy) =
         let oy = oy -. 30.0 in
         let y = y -. 40.0 in
         let dx = x -. ox in
         let dy = y -. oy in
         let ratio =
           let dy =abs_float dy in
           (dy -. 50.0) /. dy
         in
         let x = ox +. dx *. ratio in
         let y = oy +. dy *. ratio in
         Image.stroke_path
           (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
               Path.move_to p ~x:ox ~y:oy;
               Path.line_to p ~x ~y;
             )
       in
       match color with
       | None ->
         Image.seq [
           text ~halign:`CENTER ~x ~y ~size:0.6 str;
           Image.alpha 0.5 (
             Image.seq (List.map line (connect @ color_connect))
           )
         ]
       | Some color ->
         let paint = Paint.color color in
         Image.seq [
           Image.paint paint (text ~halign:`CENTER ~x ~y ~size:0.6 str);
           Image.alpha 0.5 (Image.seq [
               Image.seq (List.map line connect);
               Image.paint paint (Image.seq (List.map line color_connect))
             ]
             )
         ]
     in
     title "Traversing tree" [
       text ~size:0.8 ~x:700.0 ~y:180.0 "GPU memory";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 200.0 300.0 300.0);

       Image.paint (Paint.color Color.red)
         (Image.fill_path (fun p -> Path.rect p 700.0 200.0 300.0 60.0));
       text ~size:0.8 ~x:720.0 ~y:245.0 "Circle";
       Image.alpha 0.5 (Image.seq [
           Image.paint (Paint.color Color.green)
             (Image.fill_path (fun p -> Path.rect p 700.0 260.0 300.0 60.0));
           text ~size:0.8 ~x:720.0 ~y:305.0 "Smile";
         ]);

       text ~size:0.8 ~x:700.0 ~y:550.0 "GPU command";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 570.0 300.0 60.0);
       text ~size:0.6 ~x:720.0 ~y:610.0 "fill(circle,black,xf2)";
       (* Nodes *)
       node 300.0 600.0 ~color:Color.red "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:[300.0,600.0];
       node 100.0 400.0 "Primitive" ~connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)"
         ~color:Color.red
         ~color_connect:[300.0,600.0];
       node 280.0 400.0 "transform" ~connect:[450.0,500.0];
       node 280.0 300.0 "Primitive" ~connect:[280.0,400.0];
       node 430.0 400.0 "transform"
         ~color:Color.red
         ~color_connect:[450.0,500.0];
       node 430.0 300.0 "Primitive"
         ~color:Color.red
         ~color_connect:[430.0,400.0];
       node 580.0 400.0 "transform" ~connect:[450.0,500.0];
       node 580.0 300.0 "Primitive" ~connect:[580.0,400.0];
       node 330.0 200.0 "Circle"
         ~color:Color.red
         ~color_connect:[430.0,300.0]
         ~connect:[100.0,400.0;280.0,300.0];
       node 580.0 200.0 "Smile" ~connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ ->
     let dot =
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 15.0
         ) in
       Image.fill dot;
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       let outline = Outline.make ~cap:`ROUND ~width:15.0 () in
       Image.stroke outline path
     in
     let node ?color ?(color_connect=[]) ?(connect=[]) x y str =
       let line (ox, oy) =
         let oy = oy -. 30.0 in
         let y = y -. 40.0 in
         let dx = x -. ox in
         let dy = y -. oy in
         let ratio =
           let dy =abs_float dy in
           (dy -. 50.0) /. dy
         in
         let x = ox +. dx *. ratio in
         let y = oy +. dy *. ratio in
         Image.stroke_path
           (Outline.make ~cap:`ROUND ~width:3.0 ()) (fun p ->
               Path.move_to p ~x:ox ~y:oy;
               Path.line_to p ~x ~y;
             )
       in
       match color with
       | None ->
         Image.seq [
           text ~halign:`CENTER ~x ~y ~size:0.6 str;
           Image.alpha 0.5 (
             Image.seq (List.map line (connect @ color_connect))
           )
         ]
       | Some color ->
         let paint = Paint.color color in
         Image.seq [
           Image.paint paint (text ~halign:`CENTER ~x ~y ~size:0.6 str);
           Image.alpha 0.5 (Image.seq [
               Image.seq (List.map line connect);
               Image.paint paint (Image.seq (List.map line color_connect))
             ]
             )
         ]
     in
     let green = Color.lerp_rgba 0.5 Color.black Color.green in
     title "Traversing tree" [
       text ~size:0.8 ~x:700.0 ~y:180.0 "GPU memory";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 200.0 300.0 300.0);

       Image.alpha 0.5 (Image.seq [
           Image.paint (Paint.color Color.red)
             (Image.fill_path (fun p -> Path.rect p 700.0 200.0 300.0 60.0));
           text ~size:0.8 ~x:720.0 ~y:245.0 "Circle";
         ]);
       Image.paint (Paint.color Color.green)
         (Image.fill_path (fun p -> Path.rect p 700.0 260.0 300.0 60.0));
       text ~size:0.8 ~x:720.0 ~y:305.0 "Smile";

       text ~size:0.8 ~x:700.0 ~y:550.0 "GPU command";
       Image.stroke_path (Outline.make ~width:2.0 ())
         (fun p -> Path.rect p 700.0 570.0 300.0 60.0);
       text ~size:0.6 ~x:720.0 ~y:610.0 "fill(smile,black,xf3)";
       (* Nodes *)
       node 300.0 600.0 ~color:green "superpose";
       node 100.0 500.0 "paint(yellow)" ~connect:[300.0,600.0];
       node 100.0 400.0 "Primitive" ~connect:[100.0,500.0];
       node 450.0 500.0 "paint(black)"
         ~color:green
         ~color_connect:[300.0,600.0];
       node 280.0 400.0 "transform" ~connect:[450.0,500.0];
       node 280.0 300.0 "Primitive" ~connect:[280.0,400.0];
       node 430.0 400.0 "transform" ~connect:[450.0,500.0];
       node 430.0 300.0 "Primitive"
         ~connect:[430.0,400.0];
       node 580.0 400.0 "transform"
         ~color:green ~color_connect:[450.0,500.0];
       node 580.0 300.0 "Primitive"
         ~color:green ~color_connect:[580.0,400.0];
       node 330.0 200.0 "Circle"
         ~connect:[100.0,400.0;280.0,300.0;430.0,300.0];
       node 580.0 200.0 "Smile"
         ~color:green
         ~color_connect:[580.0,300.0];
       Image.alpha 0.8 (Image.seq [
           Image.transform
             (Transform.rescale 0.4 0.4 (Transform.translation 580.0 160.0))
             smile;
           Image.transform
             (Transform.rescale 0.6 0.6 (Transform.translation 330.0 160.0))
             dot;
         ]);
     ]
  );
  (fun _ -> title "Performance"
      [
        text ~size:0.9 ~x:80.0 ~y:195.0 "Geometry: g, Tree: n, Sharing: m";
        text ~x:80.0 ~y:300.0 "1) Processing geometry on leaves, O(g/m)";
        text ~x:80.0 ~y:385.0 "2) Other nodes have fixed size, O(1)";
        text ~x:80.0 ~y:470.0 "3) Traversal is O(n)";
        text ~x:80.0 ~y:555.0 "4) Rasterization is O(g), massively parallel";
        text ~size:0.9 ~x:80.0 ~y:660.0 "... devil is in the level of detail";
      ]
  );
]
;;
