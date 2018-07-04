open Wall
open Wall_text
open Pres_state
;;

let default_font ?(size=1.0) () =
  Font.make ~size:(64.0 *. size) font_sans
    ~placement:`Subpixel

let title =
  let color = Paint.rgba 0.0 0.0 0.0 1.0 in
  let title_fg = Font.make ~size:96.0 font_sans in
  let title_bg = Font.make ~blur:6.0 ~size:96.0 font_sans in
  fun text ->
    Image.paint color (
      Image.seq [
        simple_text title_bg text
          ~x:514.0 ~y:18.0 ~halign:`CENTER ~valign:`TOP;
        simple_text title_fg text
          ~x:512.0 ~y:16.0 ~halign:`CENTER ~valign:`TOP;
      ]
    )

let title_banner = Path.make (fun ctx ->
    Path.rect ctx ~x:0.0 ~y:0.0 ~w:1024.0 ~h:128.0
  )

let draw_arrow ~x ~y ~size color =
  Image.paint (Paint.color color)
    (Image.fill_path @@ fun t ->
     Path.move_to t x y;
     Path.line_to t (x-.size) (y+.size);
     Path.line_to t (x-.size) (y-.size);
     Path.close t;
    )

let text ?size ~x ~y str =
  simple_text ~x ~y ~halign:`LEFT ~valign:`BASELINE (default_font ?size ()) str

let text_arrow ~x ~y str =
  Image.seq [
    draw_arrow ~x ~y ~size:16.0 Color.blue;
    text ~x:(x +. 20.0) ~y:(y +. 14.0) str
  ]

;;

Slideshow.set_slides Slideshow.window [
  (fun _ ->
     Image.seq [
       Image.paint Paint.white (Image.fill rectangle);
       Image.paint background (Image.fill title_banner);
       title "The Wall library";
       text_arrow ~x:200.0 ~y:250.0 "Graphics";
       text_arrow ~x:200.0 ~y:400.0 "In OCaml";
       text_arrow ~x:200.0 ~y:550.0 "Without Pain";
     ]
  );
  (fun _ ->
     Image.seq [
       Image.paint Paint.white (Image.fill rectangle);
       Image.paint background (Image.fill title_banner);
       title "Key points";
       text ~x:150.0 ~y:250.0 "- declarative vector graphics";
       text ~x:150.0 ~y:400.0 "- rendered with OpenGL";
       text ~x:150.0 ~y:550.0 "- independent from a windowing system";
     ]
  );
  (fun _ ->
     Image.seq [
       Image.paint Paint.white (Image.fill rectangle);
       Image.paint background (Image.fill title_banner);
       title "Designed for user interfaces";
       text ~x:100.0 ~y:180.0 ~size:0.8 "Favor quality and performance, trade expressivity.";
       text ~x:100.0 ~y:400.0 "- rendered with OpenGL";
       text ~x:100.0 ~y:550.0 "- indendent from a windowing system";
     ]
  )
]
;;
