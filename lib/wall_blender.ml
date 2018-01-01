open Wall
open Wall_text

type box = Gg.box2
type font = Font.t
type label = string

(* describes the theme used to draw widgets *)

type color_offset = float (* Ranging from -1.00 to 1.00 *)

type widget = {
  outline        : color;
  item           : color;
  inner          : color;
  inner_selected : color;
  text           : color;
  text_selected  : color;
  shade_top      : color_offset;
  shade_down     : color_offset;
}

type node = {
  node_selected : color;
  wire          : color;
  wire_selected : color;
  node_text_selected : color;
  node_active   : color;
  node_backdrop : color;
  noodle_curving : float (* from 0.0 to 1.0 *);
}

type theme = {
  background       : color;
  regular          : widget;
  tool_button      : widget;
  radio_button     : widget;
  text_field       : widget;
  option           : widget;
  choice           : widget;
  number_field     : widget;
  slider           : widget;
  scrollbar        : widget;
  tooltip          : widget;
  menu             : widget;
  menu_item        : widget;
  node             : node;
}

let gray ?(a=1.0) v = Color.v v v v a

let c_0_098 = gray 0.098
let c_0_275 = gray 0.275
let c_0_353 = gray 0.353
let c_0_392 = gray 0.392
let c_0_447 = gray 0.447
let c_0_502 = gray 0.502
let c_0_600 = gray 0.600
let c_0_706 = gray 0.706
let c_0_800 = gray 0.800
let black = Color.black
let white = Color.white
let c_text = black
let c_text_selected = white

let tooltip_and_menu = {
  outline        = black;
  item           = c_0_392;
  inner          = gray ~a:0.902 0.098;
  inner_selected = gray ~a:0.902 0.176;
  text           = gray 0.627;
  text_selected  = c_text_selected;
  shade_top      = 0.0;
  shade_down     = 0.0;
}

let default_theme = {
  background = gray 0.447;

  regular = {
    outline        = c_0_098;
    item           = c_0_098;
    inner          = c_0_600;
    inner_selected = c_0_392;
    text           = c_text;
    text_selected  = c_text_selected;
    shade_top      = 0.0;
    shade_down     = 0.0;
  };

  tool_button = {
    outline        = c_0_098;
    item           = c_0_098;
    inner          = c_0_600;
    inner_selected = c_0_392;
    text           = c_text;
    text_selected  = c_text_selected;
    shade_top      = 0.15;
    shade_down     = -0.15;
  };

  radio_button = {
    outline        = black;
    item           = white;
    inner          = c_0_275;
    inner_selected = Color.v 0.337 0.502 0.761 1.0;
    text           = c_text_selected;
    text_selected  = c_text;
    shade_top      = 0.15;
    shade_down     = -0.15;
  };

  text_field = {
    outline        = c_0_098;
    item           = c_0_353;
    inner          = c_0_600;
    inner_selected = c_0_600;
    text           = c_text;
    text_selected  = c_text_selected;
    shade_top      = 0.0;
    shade_down     = 0.25;
  };

  option = {
    outline        = black;
    item           = white;
    inner          = c_0_275;
    inner_selected = c_0_275;
    text           = c_text;
    text_selected  = c_text_selected;
    shade_top      = 0.15;
    shade_down     = -0.15;
  };

  choice = {
    outline        = black;
    item           = white;
    inner          = c_0_275;
    inner_selected = c_0_275;
    text           = c_text_selected;
    text_selected  = c_0_800; (*  color_text_selected *)
    shade_top      = 0.15;
    shade_down     = -0.15;
  };

  number_field = {
    outline        = c_0_098;
    item           = c_0_353;
    inner          = c_0_706;
    inner_selected = c_0_600;
    text           = c_text;
    text_selected  = c_text_selected;
    shade_top      = -0.20;
    shade_down     = 0.0;
  };

  slider = {
    outline        = c_0_098;
    item           = c_0_502;
    inner          = c_0_706;
    inner_selected = c_0_600;
    text           = c_text;
    text_selected  = c_text_selected;
    shade_top      = -0.20;
    shade_down     = 0.0;
  };

  scrollbar = {
    outline        = gray 0.196;
    item           = c_0_502;
    inner          = gray ~a:0.706 0.314;
    inner_selected = gray ~a:0.706 0.392;
    text           = c_text;
    text_selected  = c_text_selected;
    shade_top      = 0.05;
    shade_down     = -0.05;
  };

  tooltip = tooltip_and_menu;

  menu = tooltip_and_menu;

  menu_item = {
    outline        = black;
    item           = gray ~a:0.502 0.675;
    inner          = gray ~a:0.0 0.0;
    inner_selected = Color.v 0.337 0.502 0.761 1.0;
    text           = c_text_selected;
    text_selected  = c_text;
    shade_top      = 0.38;
    shade_down     = 0.0;
  };

  node = {
    node_selected      = Color.v 0.945 0.345 0.0 1.0;
    wire               = black;
    node_text_selected = Color.v 0.498 0.439 0.439 1.0;
    node_active        = Color.v 1.0 0.667 0.251 1.0;
    wire_selected      = white;
    node_backdrop      = gray ~a:0.627 0.608;
    noodle_curving     = 0.5;
  };
}

module B2 = Gg.Box2
module P2 = Gg.P2

module Constants = struct
  let widget_height           = 21.0
  let tool_button_width       = 20.0
  let node_port_radius        = 5.0
  let node_margin_top         = 25.0
  let node_margin_down        = 5.0
  let node_margin_side        = 10.0
  let node_title_height       = 20.0
  let node_arrow_area_width   = 20.0
  let splitter_area_size      = 12.0
  let scrollbar_width         = 13.0
  let scrollbar_height        = 14.0
  let vspacing                = 1.0
  let vspacing_group          = 8.0
  let hspacing                = 8.0
  let label_font_size         = 15.0
  let pad_left                = 8.0
  let pad_right               = 8.0
  let label_separator         = ": "
  let transparent_alpha       = 0.643
  let bevel_shade             = 0.30
  let inset_bevel_shade       = 0.30
  let hover_shade             = 0.15
  let splitter_shade          = 1.0
  let icon_sheet_width        = 602
  let icon_sheet_height       = 640
  let icon_sheet_grid         = 21.0
  let icon_sheet_offset_x     = 5.0
  let icon_sheet_offset_y     = 10.0
  let icon_sheet_res          = 16.0
  let number_arrow_size       = 4.0
  let tool_radius             = 4.0
  let option_radius           = 4.0
  let option_width            = 14.0
  let option_height           = 15.0
  let text_radius             = 4.0
  let number_radius           = 10.0
  let menu_radius             = 3.0
  let shadow_feather          = 12.0
  let shadow_alpha            = 0.5
  let scrollbar_radius        = 7.0
  let scrollbar_active_shade  = 0.15
  let max_glyphs              = 1024
  let max_rows                = 32
  let text_pad_down           = 7.0
  let node_wire_outline_width = 4.0
  let node_wire_width         = 2.0
  let node_radius             = 8.0
  let node_title_feather      = 1.0
  let node_arrow_size         = 9.0
end

let minf a b : float = if a < b then a else b
let maxf a b : float = if a > b then a else b

let clampf x a b = maxf (minf x b) a

(* states altering the styling of a widget *)
type state =
  [ (* not interacting *)
    `DEFAULT
  | (* the mouse is hovering over the control *)
    `HOVER
  | (* the widget is activated (pressed) or in an active state (toggled) *)
    `ACTIVE
  ]

type corners =
  [ (* sharp top left corner *)
    `TOP_LEFT
  | (* sharp top right corner *)
    `TOP_RIGHT
  | (* sharp bottom right corner *)
    `DOWN_RIGHT
  | (* sharp bottom left corner *)
    `DOWN_LEFT
  ] list

let select_corners (corners : corners) r =
  let test x = if List.mem x corners then 0.0 else r in
  (test `TOP_LEFT, test `TOP_RIGHT, test `DOWN_LEFT, test `DOWN_RIGHT)

let offset_color color = function
  | 0.0 -> color
  | delta ->
    let delta = delta /. 2.55 in
    let f x = clampf (x +. delta) 0.0 1.0 in
    let (r,g,b,a) = Gg.V4.to_tuple color in
    Color.v (f r) (f g) (f b) a

let transparent color =
  Color.with_a color (Color.a color *. Constants.transparent_alpha)

let draw_bevel_inset ~theme box (_, _, cr2, cr3) =
  let x1  = B2.minx box in
  let y1  = B2.miny box -. 0.5 in
  let x2  = B2.maxx box in
  let y2  = B2.maxy box -. 0.5 in
  let d   = minf (B2.w box) (B2.h box) in
  let cr2 = minf cr2 (d /. 2.0) in
  let cr3 = minf cr3 (d /. 2.0) in
  let shape = Image.stroke_path (Outline.make ~width:1.0 ()) @@ fun t ->
    Path.move_to t x2 (y2-.cr2);
    Path.arc_to t x2 y2 x1 y2 cr2;
    Path.arc_to t x1 y2 x1 y1 cr3;
  in
  let bevel_color =
    offset_color theme.background Constants.inset_bevel_shade
  in
  Image.paint
    (Paint.linear_gradient
       ~sx:x1 ~sy:(y2 -. maxf cr2 cr3 -. 1.0)
       ~ex:x1 ~ey:(y2 -. 1.0)
       ~inner:(Color.with_a bevel_color 0.0)
       ~outer:bevel_color)
    shape


let draw_bevel ~theme box =
  let x1 = B2.minx box +. 0.5 and y1 = B2.miny box +. 0.5 in
  let x2 = B2.maxx box -. 0.5 and y2 = B2.maxy box -. 0.5 in
  Image.seq [
    begin
      let shape = Image.stroke_path Outline.default @@ fun t ->
        Path.move_to t ~x:x1 ~y:y2;
        Path.line_to t ~x:x2 ~y:y2;
        Path.line_to t ~x:x1 ~y:y2;
      in
      let color = offset_color theme.background (-.Constants.bevel_shade) in
      Image.paint (Paint.color (transparent color)) shape;
    end;
    begin
      let shape = Image.stroke_path Outline.default @@ fun t ->
        Path.move_to t ~x:x1 ~y:y2;
        Path.line_to t ~x:x1 ~y:y1;
        Path.line_to t ~x:x2 ~y:y1;
      in
      let color = offset_color theme.background Constants.bevel_shade in
      Image.paint (Paint.color (transparent color)) shape
    end
  ]

let inner_colors {inner; inner_selected; shade_top; shade_down} ?(flip=false) state =
  match state with
  | `DEFAULT ->
    (offset_color inner shade_top, offset_color inner shade_down)
  | `HOVER   ->
    let color = offset_color inner Constants.hover_shade in
    (offset_color color shade_top, offset_color color shade_down)
  | `ACTIVE  ->
    (offset_color inner_selected (if flip then shade_down else shade_top),
     offset_color inner_selected (if flip then shade_top else shade_down))

let rounded_box box ~corners:(cr0, cr1, cr2, cr3) =
  let w = B2.w box and h = B2.h box in
  if w > 0.0 && h > 0.0 then (
    let d = if w < h then w else h in
    let x1 = B2.minx box and y1 = B2.miny box in
    let x2 = B2.maxx box and y2 = B2.maxy box in
    Path.make @@ fun t ->
    Path.move_to t x1 (B2.midy box);
    Path.arc_to t x1 y1 x2 y1 (minf cr0 (d/.2.0));
    Path.arc_to t x2 y1 x2 y2 (minf cr1 (d/.2.0));
    Path.arc_to t x2 y2 x1 y2 (minf cr2 (d/.2.0));
    Path.arc_to t x1 y2 x1 y1 (minf cr3 (d/.2.0));
    Path.close t;
  )
  else
    Path.make ignore

let offset_box box x1 y1 x2 y2 =
  let p1 = B2.o box in
  B2.v
    (P2.v (P2.x p1 +. x1) (P2.y p1 +. y1))
    (Gg.Size2.v (B2.w box +. x2) (B2.h box +. y2))

let draw_inner_box box (cr0, cr1, cr2, cr3) inner outer =
  let x1 = B2.minx box and y1 = B2.miny box in
  let x2 = B2.maxx box and y2 = B2.maxy box in
  Image.paint
    (if B2.h box -. 2.0 > B2.w box
     then Paint.linear_gradient ~sx:x1 ~sy:y1 ~ex:x2 ~ey:y1 ~inner ~outer
     else Paint.linear_gradient ~sx:x1 ~sy:y1 ~ex:x1 ~ey:y2 ~inner ~outer)
    (Image.fill @@ rounded_box (offset_box box 1.0 1.0 (-2.0) (-3.0))
       ~corners:(max 0.0 (cr0 -. 1.0), max 0.0 (cr1 -. 1.0),
                 max 0.0 (cr2 -. 1.0), max 0.0 (cr3 -. 1.0)))

let draw_outline_box box corners color =
  let path = rounded_box (offset_box box 0.5 0.5 (-1.0) (-2.0)) ~corners in
  Image.paint
    (Paint.color color)
    (Image.stroke (Outline.make ~width:1.0 ()) path)

let draw_check ~x ~y color =
  Image.paint (Paint.color color)
    (Image.stroke_path (Outline.make ~cap:`BUTT ~join:`MITER ~width:2.0 ()) @@ fun t ->
     Path.move_to t (x+.4.0) (y+.5.0);
     Path.line_to t (x+.7.0) (y+.8.0);
     Path.line_to t (x+.14.0) (y+.1.0)
    )

let draw_up_down_arrow ~x ~y ~size color =
  let w = 1.1 *. size in
  Image.paint (Paint.color color)
    (Image.fill_path @@ fun t ->
     Path.move_to t x (y-.1.0);
     Path.line_to t (x+.0.5*.w) (y-.size-.1.0);
     Path.line_to t (x+.w) (y-.1.0);
     Path.move_to t x (y+.1.);
     Path.line_to t (x+.0.5*.w) (y+.size+.1.0);
     Path.line_to t (x+.w) (y+.1.0);
     Path.close t
    )

let draw_arrow ~x ~y ~size color =
  Image.paint (Paint.color color)
    (Image.fill_path @@ fun t ->
     Path.move_to t x y;
     Path.line_to t (x-.size) (y+.size);
     Path.line_to t (x-.size) (y-.size);
     Path.close t;
    )

let draw_node_port ~theme ~x ~y (state : [< state]) color =
  let circle = Path.make @@ fun t ->
    Path.circle t ~cx:x ~cy:y ~r:Constants.node_port_radius
  in
  Image.impose
    (Image.paint (Paint.color theme.node.wire)
       (Image.stroke (Outline.make ~width:1.0 ()) circle))
    (Image.paint
       (Paint.color
          (match state with
           | `DEFAULT -> color
           | `HOVER | `ACTIVE -> offset_color color Constants.hover_shade))
       (Image.fill circle))

let draw_colored_node_wire ~theme x0 y0 c0 x1 y1 c1 =
  let length = maxf (abs_float (x1 -. x0)) (abs_float (y1 -. y0)) in
  let delta = length *. theme.node.noodle_curving in
  let path = Path.make @@ fun t ->
    Path.move_to t x0 y0;
    Path.bezier_to t
      ~c1x:(x0 +. delta) ~c1y:y0 ~c2x:(x1 -. delta) ~c2y:y1 ~x:x1 ~y:y1
  in
  let colorw = Color.with_a theme.node.wire (minf (Color.a c0) (Color.a c1)) in
  Image.impose
    (Image.paint (Paint.color colorw)
       (Image.stroke (Outline.make ~width:Constants.node_wire_outline_width ()) path))
    (Image.paint (Paint.linear_gradient x0 y0 x1 y1 c0 c1)
       (Image.stroke (Outline.make ~width:Constants.node_wire_width ()) path))

let midgray = gray 0.5

let node_wire_color =
  fun theme -> function
    | `DEFAULT -> midgray
    | `HOVER   -> theme.wire_selected
    | `ACTIVE  -> theme.node_active

let draw_node_wire ~theme x0 y0 s0 x1 y1 s1 =
  draw_colored_node_wire ~theme
    x0 y0 (node_wire_color theme.node s0)
    x1 y1 (node_wire_color theme.node s1)

let b2_with_h box h =
  let tl = B2.o box and w = B2.w box in
  B2.v tl (Gg.Size2.v w h)

let draw_drop_shadow box ~r ~feather ~alpha =
  let shape = Image.fill_path @@ fun t ->
    let x1 = B2.minx box and y1 = B2.miny box in
    let x2 = B2.maxx box and y2 = B2.maxy box in
    Path.move_to t (x1 -. feather) y1;
    Path.line_to t x1 y1;
    Path.line_to t x1 (y2 -. feather);
    Path.arc_to t ~x1 ~y1:y2 ~x2:(x1+.r) ~y2 ~r;
    Path.arc_to t ~x1:x2 ~y1:y2 ~x2 ~y2:(y2-.r) ~r;
    Path.line_to t x2 y1;
    Path.line_to t (x2+.feather) y1;
    Path.line_to t (x2+.feather) (y2+.feather);
    Path.line_to t (x1-.feather) (y2+.feather);
    Path.close t
  in
  let x1 = B2.minx box and y1 = B2.miny box in
  let x2 = B2.maxx box and y2 = B2.maxy box in
  let paint = Paint.box_gradient
      ~x:(x1 -. feather *. 0.5)
      ~y:(y1 +. feather *. 0.5)
      ~w:(x2 -. x1 +. feather)
      ~h:(y2 -. y1)
      ~r:(r +. feather *. 0.5)
      ~f:feather
      ~inner:(gray ~a:(alpha*.alpha) 0.0)
      ~outer:(gray ~a:0.0 0.0)
  in
  Image.paint paint shape

let draw_tooltip_background ~theme box =
  let shade_top, shade_down = inner_colors theme.tooltip `DEFAULT in
  let corners = Constants.(menu_radius,menu_radius,menu_radius,menu_radius) in
  let box' = offset_box box 0.0 0.0 0.0 1.0 in
  Image.seq [
    draw_inner_box box' corners shade_top shade_down;
    draw_outline_box box' corners (transparent theme.tooltip.outline);
    draw_drop_shadow box ~r:Constants.menu_radius
      ~feather:Constants.shadow_feather ~alpha:Constants.shadow_alpha
  ]

type icon = {
  tex: Texture.t;
  x: int;
  y: int;
  w: int;
  h: int;
}

let draw_icon x y icon =
  let shape = Image.fill_path @@ fun t ->
    Path.rect t ~x ~y ~w:(float icon.w) ~h:(float icon.h)
  in
  let paint = Paint.image_pattern
      (P2.v (float icon.x) (float icon.y))
      (Gg.Size2.v
         (float (Texture.width icon.tex))
         (float (Texture.height icon.tex)))
      ~angle:0.0 ~alpha:1.0
      icon.tex
  in
  Image.paint paint shape

let draw_node_icon_label box ?icon c0 c1 ~align ~font label =
  Image.impose
    begin match font, label with
      | Some font, Some label ->
        let font' = {font with Font.blur = Constants.node_title_feather} in
        Image.impose
          (Image.paint (Paint.color c1)
             (simple_text font' label
                ~halign:`LEFT ~valign:`BASELINE
                ~x:(B2.minx box +. 1.0)
                ~y:(B2.maxy box +. 3.0 -. Constants.text_pad_down)))
          (Image.paint (Paint.color c0)
             (simple_text font label
                ~halign:`LEFT ~valign:`BASELINE
                ~x:(B2.minx box +. 0.0)
                ~y:(B2.maxy box +. 2.0 -. Constants.text_pad_down)))
      | _ -> Image.empty
    end
    begin match icon with
      | None -> Image.empty
      | Some icon ->
        draw_icon (B2.maxx box -. float icon.w) (B2.miny box +. 3.0) icon
    end

let draw_node_background ~theme box state ?icon ?font ?label color =
  Image.seq [
    draw_inner_box
      (b2_with_h box (Constants.node_title_height +. 2.0))
      Constants.(node_radius,node_radius,0.0,0.0)
      (transparent (offset_color color Constants.bevel_shade))
      (transparent color);
    draw_inner_box
      (offset_box box
         0.0 (Constants.node_title_height -. 1.0)
         0.0 (2.0 -. Constants.node_title_height))
      Constants.(0.0,0.0,node_radius,node_radius)
      (transparent theme.node.node_backdrop)
      (transparent theme.node.node_backdrop);
    draw_node_icon_label
      (offset_box (b2_with_h box Constants.node_title_height)
         Constants.node_arrow_area_width 0.0
         Constants.(-. node_arrow_area_width -. node_margin_side) 0.0)
      theme.regular.text
      (offset_color color Constants.bevel_shade)
      ?icon ~align:`LEFT ~font
      label;
    begin
      let border_color, arrow_color = match state with
        | `DEFAULT -> Color.black, offset_color color (-. Constants.bevel_shade)
        | `HOVER   -> (theme.node.node_selected, theme.node.node_selected)
        | `ACTIVE  -> (theme.node.node_active, theme.node.node_selected)
      in
      draw_outline_box (offset_box box 0.0 0.0 0.0 1.0)
        Constants.(node_radius,node_radius,node_radius,node_radius)
        (transparent border_color)
    end;
    draw_drop_shadow box ~r:Constants.node_radius
      ~feather:Constants.shadow_feather ~alpha:Constants.shadow_alpha
  ]

let draw_splitter_widgets ~theme box =
  let inset = transparent theme.background in
  let inset_light =
    transparent (offset_color theme.background Constants.splitter_shade)
  and inset_dark =
    transparent (offset_color theme.background (-. Constants.splitter_shade))
  in
  let x1 = B2.minx box and y1 = B2.miny box
  and x2 = B2.maxx box and y2 = B2.maxy box in
  Image.seq [
    Image.paint (Paint.color inset_dark)
      (Image.stroke_path Outline.default @@ fun t ->
       Path.move_to t (x1 +. 0.0 ) (y2 -. 13.0);
       Path.line_to t (x1 +. 13.0) (y2 +. 0.0 );
       Path.move_to t (x1        ) (y2 -. 9.0 );
       Path.line_to t (x1 +. 9.0 ) (y2        );
       Path.move_to t (x1        ) (y2 -. 5.0 );
       Path.line_to t (x1 +. 5.0 ) (y2        );
       Path.move_to t (x2 -. 11.0) (y1        );
       Path.line_to t (x2        ) (y1 +. 11.0);
       Path.move_to t (x2 -. 7.0 ) (y1        );
       Path.line_to t (x2        ) (y1 +. 7.0 );
       Path.move_to t (x2 -. 3.0 ) (y1        );
       Path.line_to t (x2        ) (y1 +. 3.0 );
      );
    Image.paint (Paint.color inset_light)
      (Image.stroke_path Outline.default @@ fun t ->
       Path.move_to t (x1        ) (y2 -. 11.0);
       Path.line_to t (x1 +. 11.0) (y2        );
       Path.move_to t (x1        ) (y2 -. 7.0 );
       Path.line_to t (x1 +. 7.0 ) (y2        );
       Path.move_to t (x1        ) (y2 -. 3.0 );
       Path.line_to t (x1 +. 3.0 ) (y2        );
       Path.move_to t (x2 -. 13.0) (y1        );
       Path.line_to t (x2        ) (y1 +. 13.0);
       Path.move_to t (x2 -. 9.0 ) (y1        );
       Path.line_to t (x2        ) (y1 +. 9.0 );
       Path.move_to t (x2 -. 5.0 ) (y1        );
       Path.line_to t (x2        ) (y1 +. 5.0 );
      );
    Image.paint (Paint.color inset)
      (Image.stroke_path Outline.default @@ fun t ->
       Path.move_to t (x1        ) (y2 -. 12.0);
       Path.line_to t (x1 +. 12.0) (y2        );
       Path.move_to t (x1        ) (y2 -. 8.0 );
       Path.line_to t (x1 +. 8.0 ) (y2        );
       Path.move_to t (x1        ) (y2 -. 4.0 );
       Path.line_to t (x1 +. 4.0 ) (y2        );
       Path.move_to t (x2 -. 12.0) (y1        );
       Path.line_to t (x2        ) (y1 +. 12.0);
       Path.move_to t (x2 -. 8.0 ) (y1        );
       Path.line_to t (x2        ) (y1 +. 8.0 );
       Path.move_to t (x2 -. 4.0 ) (y1        );
       Path.line_to t (x2        ) (y1 +. 4.0 );
      )
  ]

let draw_join_area_overlay box direction =
  let vertical, mirror = match direction with
    | `Left  -> false, true
    | `Right -> false, false
    | `Up    -> true, true
    | `Down  -> true, false
  in
  let x = B2.minx box and y = B2.miny box in
  let w, h =
    let w = B2.w box and h = B2.h box in
    if vertical then h, w else w, h
  in
  let s = minf w h in
  let x0, y0, x1, y1, s =
    if mirror
    then (w, h, 0.0, 0.0, -.s)
    else (0.0, 0.0, w, h, s)
  in
  let s2 = s /. 2.0 and s4 = s /. 4.0 and s8 = s /. 8.0 in
  let yc = (y0 +. y1) *. 0.5 and x4 = x0 +. s4 in
  let points = [|
    x0       ; y0       ;
    x1       ; y0       ;
    x1       ; y1       ;
    x0       ; y1       ;
    x0       ; yc +. s8 ;
    x4       ; yc +. s8 ;
    x4       ; yc +. s4 ;
    x0 +. s2 ; yc       ;
    x4       ; yc -. s4 ;
    x4       ; yc -. s8 ;
    x0       ; yc -. s8 ;
  |] in
  let path = Image.fill_path @@ fun t ->
    let vertical = if vertical then 1 else 0 in
    Path.move_to t (x +. points.(vertical)) (y +. points.(1-vertical));
    for i = 1 to Array.length points / 2 - 1 do
      Path.line_to t
        (x +. points.(2 * i + vertical))
        (y +. points.(2 * i + 1 - vertical))
    done
  in
  Image.paint (Paint.color (gray ~a:0.3 0.0)) path

let b2 x y w h = B2.v (P2.v x y) (Gg.Size2.v w h)

let draw_icon_label_value box ?font ?icon ?(halign=`LEFT) ?valign ?label ?value color =
  let x = B2.minx box and y = B2.miny box in
  match font, label with
  | Some font, Some label ->
    let x = x +. Constants.pad_left in
    let icon, x = match icon with
      | None -> (Image.empty, x)
      | Some icon ->
        (draw_icon (x +. 4.0) (y +. 2.0) icon, x +. float icon.w)
    in
    let paint = Paint.color color in
    let text = match value with
      | Some value -> label ^ Constants.label_separator ^ value
      | None -> label
    in
    let x = match halign with
      | `LEFT -> x
      | `CENTER -> B2.midx box
      | `RIGHT -> B2.maxx box -. Constants.pad_right
    in
    Image.impose icon
      (Image.paint paint
         (simple_text ~halign ?valign font
            ~x ~y:(y +. Constants.widget_height -. Constants.text_pad_down)
            text))
  | _, _ -> begin match icon with
      | None -> Image.empty
      | Some icon -> draw_icon (x +. 2.0) (y +. 2.0) icon
    end

let text_color theme = function
  | `ACTIVE -> theme.text_selected
  | `DEFAULT | `HOVER -> theme.text

let draw_tool_button ~theme box ~corners state ?icon ~font text =
  let corners = select_corners corners Constants.text_radius in
  let (shade_top, shade_down) = inner_colors theme.tool_button state ~flip:true in
  Image.seq [
    draw_bevel_inset ~theme box corners;
    draw_inner_box box corners shade_top shade_down;
    draw_outline_box box corners theme.tool_button.outline;
    draw_icon_label_value box ?icon ~halign:`CENTER ~font ~label:text
      (text_color theme.tool_button state)
  ]

let draw_radio_button ~theme box ~corners state ?icon ~font label =
  let corners = select_corners corners Constants.option_radius in
  let shade_top, shade_down = inner_colors theme.radio_button state ~flip:true in
  Image.seq [
    draw_bevel_inset ~theme box corners;
    draw_inner_box box corners shade_top shade_down;
    draw_outline_box box corners (transparent theme.radio_button.outline);
    draw_icon_label_value box ~halign:`CENTER ?icon ~font ~label
      (text_color theme.radio_button state)
  ]

let draw_label ~theme ~font box ?icon label =
  draw_icon_label_value box ?icon ~label ~font theme.regular.text

let draw_background ~theme box =
  Image.paint (Paint.color theme.background)
    (Image.fill_path @@ fun t ->
     Path.rect t (B2.minx box) (B2.miny box) (B2.w box) (B2.h box))

let draw_node_arrow_down ~x ~y ~size color =
  Image.paint (Paint.color color)
    (Image.fill_path @@ fun t ->
     Path.move_to t x y;
     Path.line_to t (x +. size *. 0.5) (y -. size);
     Path.line_to t (x -. size *. 0.5) (y -. size);
     Path.close t
    )

let draw_menu_item ~theme box state ?icon ~font label =
  let menu_item = theme.menu_item in
  let base, state =
    match state with
    | `DEFAULT -> (Image.empty, `DEFAULT)
    | `ACTIVE | `HOVER ->
      (draw_inner_box box (0.0,0.0,0.0,0.0)
         (offset_color menu_item.inner_selected menu_item.shade_top)
         (offset_color menu_item.inner_selected menu_item.shade_down),
       `ACTIVE)
  in
  Image.impose base
    (draw_icon_label_value box ?icon ~font ~label
       (text_color menu_item state) ~halign:`LEFT)

let draw_menu_background ~theme ~corners box =
  let corners = select_corners corners Constants.menu_radius in
  let shade_top, shade_down = inner_colors theme.menu `DEFAULT in
  let box' = offset_box box 0.0 0.0 0.0 1.0 in
  Image.seq [
    draw_inner_box box' corners shade_top shade_down;
    draw_outline_box box' corners (transparent theme.menu.outline);
    draw_drop_shadow box
      ~r:Constants.menu_radius
      ~feather:Constants.shadow_feather
      ~alpha:Constants.shadow_alpha
  ]

let draw_menu_label ~theme box ~font ?icon label =
  draw_icon_label_value box theme.menu.text
    ?icon ~halign:`LEFT ~font ~label

let draw_scroll_bar ~theme box state ~offset ~size =
  let scrollbar = theme.scrollbar in
  let corners =
    Constants.(scrollbar_radius,scrollbar_radius,scrollbar_radius,scrollbar_radius) in
  let base =
    Image.seq [
      draw_bevel_inset ~theme box corners;
      draw_inner_box box corners
        (offset_color scrollbar.inner (3.0 *. scrollbar.shade_down))
        (offset_color scrollbar.inner (3.0 *. scrollbar.shade_top));
      draw_outline_box box corners
        (transparent scrollbar.outline);
    ]
  in
  let scroll_handle_rect box ~offset ~size =
    let size = clampf size 0.0 1.0 in
    let offset = clampf offset 0.0 1.0 in
    let x = B2.minx box and y = B2.miny box in
    let w = B2.w box and h = B2.h box in
    if h > w then (
      let hs = maxf (size *. h) (w +. 1.0) in
      B2.v (P2.v x (y +. (h -. hs) *. offset))
        (Gg.Size2.v w hs)
    ) else (
      let ws = maxf (size *. w) (h -. 1.0) in
      B2.v (P2.v (x +. (w -. ws) *. offset) y)
        (Gg.Size2.v ws h)
    )
  in
  let box = scroll_handle_rect box ~offset ~size in
  let item_color = scrollbar.item in
  let item_color = match state with
    | `ACTIVE -> offset_color item_color Constants.scrollbar_active_shade
    | `DEFAULT | `HOVER -> item_color
  in
  Image.seq [
    base;
    draw_inner_box box corners
      (offset_color item_color (3.0 *. scrollbar.shade_top))
      (offset_color item_color (3.0 *. scrollbar.shade_down));
    draw_outline_box box corners
      (transparent scrollbar.outline)
  ]

let draw_text_field ~theme box ~corners state ?icon ~font ?caret text =
  let corners = select_corners corners Constants.text_radius in
  let shade_top, shade_down = inner_colors theme.text_field state in
  Image.seq [
    draw_bevel_inset ~theme box corners;
    draw_inner_box box corners shade_top shade_down;
    draw_outline_box box corners
      (transparent theme.text_field.outline);
    (* FIXME
       let caret = if state <> `ACTIVE then (fst caret, -1) else caret in
       draw_icon_label_caret ~theme box ?icon ~font
         theme.(text_color text_field state)
         text theme.(text_field.item) ~caret *)
    draw_icon_label_value box ?icon ~font
      (text_color theme.text_field state)
      ~label:text
  ]

let draw_option_button ~theme box state ~font label =
  let ox = B2.minx box in
  let oy = B2.maxy box -. Constants.option_height -. 3.0 in
  let box' = B2.v (P2.v ox oy)
      Constants.(Gg.Size2.v option_width option_height) in
  let corners =
    Constants.(option_radius, option_radius, option_radius, option_radius) in
  let shade_top, shade_down = inner_colors theme.option state ~flip:true in
  Image.seq [
    draw_bevel_inset ~theme box' corners;
    draw_inner_box box' corners shade_top shade_down;
    draw_outline_box box' corners
      (transparent theme.option.outline);
    (match state with
     | `ACTIVE -> draw_check (transparent theme.option.item) ~x:ox ~y:oy
     | `HOVER | `DEFAULT -> Image.empty);
    draw_icon_label_value (offset_box box 12.0 0.0 (-12.0) (-1.0))
      (text_color theme.option state)
      ~halign:`LEFT ~font ~label
  ]

let draw_choice_button ~theme box ~corners ~font state ?icon label =
  let corners = select_corners corners Constants.option_radius in
  let shade_top, shade_down = inner_colors theme.choice state ~flip:true in
  let x = B2.maxx box -. 10.0 and y = B2.miny box +. 10.0 in
  Image.seq [
    draw_bevel_inset ~theme box corners;
    draw_inner_box box corners shade_top shade_down;
    draw_outline_box box corners (transparent theme.choice.outline);
    draw_icon_label_value box ?icon
      (text_color theme.choice state)
      ~halign:`LEFT ~font ~label;
    draw_up_down_arrow ~x ~y ~size:5.0
      (transparent theme.choice.item)
  ]

let draw_color_button ~theme box ~corners color =
  let corners = select_corners corners Constants.tool_radius in
  Image.seq [
    draw_bevel_inset ~theme box corners;
    draw_inner_box box corners color color;
    draw_outline_box box corners
      (transparent theme.tool_button.outline)
  ]

let draw_number_field ~theme box ~corners state ~font ~label ~value =
  let corners = select_corners corners Constants.number_radius in
  let shade_top, shade_down = inner_colors theme.number_field state ~flip:false in
  let y = B2.miny box +. 10.0 in
  let x1 = B2.minx box +. 8.0 and x2 = B2.maxx box -. 8.0 in
  Image.seq [
    draw_bevel_inset ~theme box corners;
    draw_inner_box box corners shade_top shade_down;
    draw_outline_box box corners (transparent theme.number_field.outline);
    draw_icon_label_value box
      (text_color theme.number_field state)
      ~halign:`CENTER ~font ~label ~value;
    draw_arrow ~x:x1 ~y ~size:(-.Constants.number_arrow_size)
      (transparent theme.number_field.item);
    draw_arrow ~x:x2 ~y ~size:Constants.number_arrow_size
      (transparent theme.number_field.item)
  ]

let draw_slider ~theme box ~corners state ?icon ?font ?label ?value progress =
  let slider = theme.slider in
  let corners = select_corners corners Constants.number_radius in
  let shade_top, shade_down = inner_colors theme.slider state in
  Image.seq [
    draw_bevel_inset ~theme box corners;
    draw_inner_box box corners shade_top shade_down;
    let shade_top = offset_color slider.item slider.shade_top in
    let shade_down = offset_color slider.item slider.shade_down in
    let shade_top, shade_down =
      match state with
      | `ACTIVE -> (shade_top, shade_down)
      | `DEFAULT | `HOVER -> (shade_down, shade_top)
    in
    let x = B2.minx box and y = B2.miny box and w = B2.w box and h = B2.h box in
    Image.seq [
      Image.intersect_scissor (b2 x y (8.+.(w-.8.)*.progress) h)
        (draw_inner_box box corners shade_top shade_down);
      draw_outline_box box corners (transparent slider.outline);
      draw_icon_label_value box
        (text_color theme.slider state)
        ~halign:`CENTER ?icon ?font ?label ?value
    ]
  ]

let node_port ?(theme=default_theme) ~x ~y state color =
  draw_node_port ~theme ~x ~y state color

let colored_node_wire ?(theme=default_theme) ~x1 ~y1 ~c1 ~x2 ~y2 ~c2 =
  draw_colored_node_wire ~theme x1 y1 c1 x2 y2 c2

let node_wire ?(theme=default_theme) ~x1 ~y1 ~s1 ~x2 ~y2 ~s2 =
  draw_node_wire ~theme x1 y1 s1 x2 y2 s2

let drop_shadow box ~r ~feather ~alpha =
  draw_drop_shadow box ~r ~feather ~alpha

let tooltip_background ?(theme=default_theme) box =
  draw_tooltip_background ~theme box

let node_background ?(theme=default_theme) box state ?icon ?font ?label color =
  draw_node_background ~theme box ?icon ?font ?label state color

let splitter_widgets ?(theme=default_theme) box =
  draw_splitter_widgets ~theme box

let join_area_overlay box dir =
  draw_join_area_overlay box dir

let tool_button ?(theme=default_theme) box ?(corners=[]) state ?icon ~font text =
  draw_tool_button ~theme box ~corners state ?icon ~font text

let radio_button ?(theme=default_theme) box ?(corners=[]) state ?icon ~font text =
  draw_radio_button ~theme box ~corners state ?icon ~font text

let label ?(theme=default_theme) box ?icon ~font label =
  draw_label ~theme box ?icon ~font label

let background ?(theme=default_theme) box =
  draw_background ~theme box

let node_arrow_down ~x ~y ~size color =
  draw_node_arrow_down ~x ~y ~size color

let menu_item ?(theme=default_theme) box state ?icon ~font label =
  draw_menu_item ~theme box state ?icon ~font label

let menu_background ?(theme=default_theme) ?(corners=[]) box =
  draw_menu_background ~theme box ~corners

let menu_label ?(theme=default_theme) box ?icon ~font label =
  draw_menu_label ~theme box ?icon ~font label

let scroll_bar ?(theme=default_theme) box state ~offset ~size =
  draw_scroll_bar ~theme box state ~offset ~size

let text_field ?(theme=default_theme) box ?(corners=[]) state ?icon ~font ?caret label =
  draw_text_field ~theme box ~corners state ?icon ~font ?caret label

let option_button ?(theme=default_theme) box state ~font label =
  draw_option_button ~theme box state ~font label

let choice_button ?(theme=default_theme) box ?(corners=[]) state ?icon ~font label =
  draw_choice_button ~theme box ~corners state ?icon ~font label

let color_button ?(theme=default_theme) box ?(corners=[]) color =
  draw_color_button ~theme box ~corners color

let number_field ?(theme=default_theme) box ?(corners=[]) state ~font ~label ~value =
  draw_number_field ~theme box ~corners state ~font ~label ~value

let slider ?(theme=default_theme) box ?(corners=[]) state ?icon ?font ?label ?value progress =
  draw_slider ~theme box ~corners state ?icon ?font ?label ?value progress
