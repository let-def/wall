open Wall
open Wall_text

type box = Gg.box2
type color_offset = float (* Ranging from -1.00 to 1.00 *)
type font = Font.t
type label = string

type widget = {
  outline         : color;
  item            : color;
  inner           : color;
  inner_selected  : color;
  text            : color;
  text_selected   : color;
  shade_top       : color_offset;
  shade_down      : color_offset;
}

type node = {
  node_selected      : color;
  wire               : color;
  wire_selected      : color;
  node_text_selected : color;
  node_active        : color;
  node_backdrop      : color;
  noodle_curving     : float;
}

type theme = {
  background   : color;
  regular      : widget;
  tool_button  : widget;
  radio_button : widget;
  text_field   : widget;
  option       : widget;
  choice       : widget;
  number_field : widget;
  slider       : widget;
  scrollbar    : widget;
  tooltip      : widget;
  menu         : widget;
  menu_item    : widget;
  node         : node;
}

val default_theme : theme

module Constants :
sig
  val widget_height : float
  val tool_button_width : float
  val node_port_radius : float
  val node_margin_top : float
  val node_margin_down : float
  val node_margin_side : float
  val node_title_height : float
  val node_arrow_area_width : float
  val splitter_area_size : float
  val scrollbar_width : float
  val scrollbar_height : float
  val vspacing : float
  val vspacing_group : float
  val hspacing : float
  val label_font_size : float
  val pad_left : float
  val pad_right : float
  val label_separator : string
  val transparent_alpha : float
  val bevel_shade : float
  val inset_bevel_shade : float
  val hover_shade : float
  val splitter_shade : float
  val icon_sheet_width : int
  val icon_sheet_height : int
  val icon_sheet_grid : float
  val icon_sheet_offset_x : float
  val icon_sheet_offset_y : float
  val icon_sheet_res : float
  val number_arrow_size : float
  val tool_radius : float
  val option_radius : float
  val option_width : float
  val option_height : float
  val text_radius : float
  val number_radius : float
  val menu_radius : float
  val shadow_feather : float
  val shadow_alpha : float
  val scrollbar_radius : float
  val scrollbar_active_shade : float
  val max_glyphs : int
  val max_rows : int
  val text_pad_down : float
  val node_wire_outline_width : float
  val node_wire_width : float
  val node_radius : float
  val node_title_feather : float
  val node_arrow_size : float
end

type state = [ `ACTIVE | `DEFAULT | `HOVER ]
type corners = [ `DOWN_LEFT | `DOWN_RIGHT | `TOP_LEFT | `TOP_RIGHT ] list
type icon = { tex : Texture.t; x : int; y : int; w : int; h : int }

val node_port : ?theme:theme ->
  x:float -> y:float -> [< state] -> color -> Image.t

val colored_node_wire : ?theme:theme ->
  x1:float -> y1:float -> c1:color ->
  x2:float -> y2:float -> c2:color -> Image.t

val node_wire : ?theme:theme ->
  x1:float -> y1:float -> s1:[< state] ->
  x2:float -> y2:float -> s2:[< state] -> Image.t

val drop_shadow : box -> r:float -> feather:float -> alpha:float -> Image.t

val tooltip_background : ?theme:theme -> box -> Image.t

val node_background : ?theme:theme -> box -> [< state] ->
  ?icon:icon -> ?font:font -> ?label:label -> color -> Image.t

val splitter_widgets : ?theme:theme -> box -> Image.t

val join_area_overlay : box -> [`Left | `Right | `Up | `Down] -> Image.t

val tool_button : ?theme:theme -> box -> ?corners:corners -> [< state] ->
  ?icon:icon -> font:font -> label -> Image.t

val radio_button : ?theme:theme -> box -> ?corners:corners -> [< state] ->
  ?icon:icon -> font:font -> label -> Image.t

val label : ?theme:theme -> box ->
  ?icon:icon -> font:font -> label -> Image.t

val background : ?theme:theme -> box -> Image.t

val node_arrow_down :
  x:float -> y:float -> size:float -> color -> Image.t

val menu_item : ?theme:theme -> box ->
  [< state] -> ?icon:icon -> font:font -> label -> Image.t

val menu_background : ?theme:theme -> ?corners:corners -> box -> Image.t

val menu_label : ?theme:theme -> box ->
  ?icon:icon -> font:font -> label -> Image.t

val scroll_bar : ?theme:theme -> box ->
  [< state] -> offset:float -> size:float -> Image.t

val text_field : ?theme:theme -> box -> ?corners:corners ->
  [< state] -> ?icon:icon -> font:font -> ?caret:int -> label -> Image.t

val option_button : ?theme:theme -> box ->
  [< state] -> font:font -> label -> Image.t

val choice_button : ?theme:theme -> box -> ?corners:corners ->
  [< state] -> ?icon:icon -> font:font -> label -> Image.t

val color_button : ?theme:theme -> box -> ?corners:corners -> color -> Image.t

val number_field : ?theme:theme -> box -> ?corners:corners ->
  [< state] -> font:font -> label:label -> value:label -> Image.t

val slider : ?theme:theme -> box -> ?corners:corners ->
  [< state] -> ?icon:icon -> ?font:font -> ?label:label -> ?value:label -> float -> Image.t
