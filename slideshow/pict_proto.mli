type slide = Slideshow.slide

type t

val t : string -> t
val tt : string -> t

val p : ?align:[`left|`center|`right] -> ?fill:bool -> ?width:float -> string -> t
val frame : t -> t
val slide : ?title:string -> ?align:[`top|`mid|`bot] -> t list -> slide
