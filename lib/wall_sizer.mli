type len = float

type rect = {
  x : float;
  y : float;
  w : float;
  h : float;
}

type line = {
  x : float;
  y : float;
  h : float;
  w : float;
  d : float;
}

type par =
  | Simple of line
  | Complex of { top : line; body : rect; bot : line }

type dim

val dim : ?shrink:len -> ?stretch:len -> ?fill:float -> len -> dim

type 'a spec

val ideal_size : 'a spec -> len * len
val minimal_size : 'a spec -> len * len

module Rect : sig
  val spec : width:dim -> height:dim -> rect spec

  type gravity = [
    | `TOP
    | `ABOVE
    | `CENTER
    | `BELOW
    | `BOTTOM
  ]

  val line  : gravity -> rect spec -> line spec
  val line' : gravity -> rect spec -> line -> rect
  val par   : gravity -> rect spec -> par spec
  val par'  : gravity -> rect spec -> par -> rect
end

module Line : sig
  val spec : width:dim -> height:dim -> depth:dim -> line spec

  val concat  : line spec -> line spec -> line spec
  val concat' : line spec -> line spec -> line -> line * line
  val rect    : line spec -> rect spec
  val rect'   : line spec -> rect -> line
  val par     : line spec -> par spec
  val par'    : line spec -> par -> line
end

module Par : sig
  type gravity = [`TOP | `BOTTOM]
  val lf : par spec
  val empty : par spec

  val rect     : par spec -> rect spec
  val rect'    : par spec -> rect -> par
  val line     : gravity -> par spec -> line spec
  val line'    : gravity -> par spec -> line -> par
  val concat   : par spec -> par spec -> par spec
  val concat'  : par spec -> par spec -> par -> par * par
  val prepend  : line spec -> par spec -> par spec
  val prepend' : line spec -> par spec -> par -> line * par
  val append   : par spec -> line spec -> par spec
  val append'  : par spec -> line spec -> par -> par * line
end

module Op : sig
  type ('a, 'b) unary
  type ('a, 'b, 'c) binary

  val apply_unary : ('a, 'b) unary -> 'a spec -> 'b spec
  val solve_unary : ('a, 'b) unary -> 'a spec -> 'b -> 'a

  val id : ('a, 'a) unary
  val compose : ('a, 'b) unary -> ('b, 'c) unary -> ('a, 'c) unary

  (*val compose_fst : ('a, 'z) unary -> ('z, 'b, 'c) binary -> ('a, 'b, 'c) binary *)
  (*val compose_snd : ('b, 'z) unary -> ('a, 'z, 'c) binary -> ('a, 'b, 'c) binary *)

  val apply_binary : ('a, 'b, 'c) binary -> 'a spec -> 'b spec -> 'c spec
  val solve_binary : ('a, 'b, 'c) binary -> 'a spec -> 'b spec -> 'c -> 'a * 'b
end

module Ops : sig
  val line_of_rect : Rect.gravity -> (rect, line) Op.unary
  val par_of_rect  : Rect.gravity -> (rect, par) Op.unary

  val line_concat  : (line, line, line) Op.binary
  val rect_of_line : (line, rect) Op.unary
  val par_of_line  : (line, par ) Op.unary

  val rect_of_par  : (par, rect) Op.unary
  val line_of_par  : Par.gravity -> (par, line) Op.unary
  val par_concat   : (par , par , par) Op.binary
  val par_prepend  : (line, par , par) Op.binary
  val par_append   : (par , line, par) Op.binary

  val concat : ('a, 'a, 'a) Op.binary
end
