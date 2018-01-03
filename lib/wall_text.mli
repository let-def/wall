open Wall

val utf8_decode : int ref -> string -> int
(** [utf8_decode r s] returns the unicode codepoint starting at offset [!r],
    advancing [r] to the beginning of next codepoint ot [String.length s] when
    the end is reached.
    If the string was not properly encoded, [-1] is returned and [r] is
    advanced to hopefully resume parsing. *)

module Font : sig
  type glyph_placement = [ `Aligned | `Subpixel ]

  type t = {
    glyphes     : Stb_truetype.t;
    size        : float;
    blur        : float;
    spacing     : float;
    line_height : float;
    placement   : glyph_placement;
  }

  val make:
    ?size:float -> ?blur:float -> ?spacing:float -> ?line_height:float ->
    ?placement:glyph_placement -> Stb_truetype.t -> t

  type metrics = {
    ascent   : float;
    descent  : float;
    line_gap : float;
  }

  val font_metrics: t -> metrics

  val text_width: t -> string -> float

  type measure = {
    width : float;
    height : float;
    depth : float;
  }

  val text_measure : t -> string -> measure
end

type simple_typesetter = (Font.t * Gg.p2 * string) typesetter

val simple_typesetter : unit -> simple_typesetter

(** [simple_text ?frame ?halign ?valign font ~x ~y text] is a shape that
    represents [text] drawn using [font] at position [x,y].

    The optionals [halign] and [valign] arguments describe how the text should
    be positioned w.r.t point [x,y].

    halign values:
    - [`LEFT], text will start at coordinate [x] ([x] is the leftmost point)
    - [`CENTER], text will be centered around [x]
    - [`RIGHT], text will end at coordinate [x] ([x] is the rightmost point)

    valign values:
    - [`TOP], top of the text will be at coordinate [y], drawing will go below
    - [`MIDDLE], text will be vertically centered at coordinate [y]
    - [`BOTTOM], bottom of the text will be at coordinate [y], drawing will be
      above
    - [`BASELINE], the baseline of the text will be at coordinate [y], most
      letters will be above but descender (as in letters such as y, p, j, q)
      will go below.
*)
val simple_text
  :  ?typesetter:simple_typesetter
  -> ?halign:[`LEFT | `CENTER | `RIGHT]
  -> ?valign:[`TOP | `MIDDLE | `BOTTOM | `BASELINE]
  -> Font.t -> x:float -> y:float -> string -> Wall.image
