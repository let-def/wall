type line = {
  x : float;
  y : float;
  h : float;
  w : float;
  d : float;
}

type rect = {
  x : float;
  y : float;
  w : float;
  h : float;
}

type len = float

type dim
val dim : ?shrink:len -> ?stretch:len -> ?fill:float -> len -> dim

type box
val atom : width:dim -> height:dim -> depth:dim -> box

type par
val par : box -> par
val box : par -> box
val lf : par
val empty : par
val (--) : par -> par -> par

val ideal_size : par -> len * len
val minimal_size : par -> len * len

type page
val page : ?x:len -> ?y:len -> width:len -> height:len -> par -> page
val split : page -> par -> par -> page * page

val page_line : page -> line
val page_rect : page -> rect
