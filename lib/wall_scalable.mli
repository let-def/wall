open Wall
type measure = Wall_text.Font.measure
type size = Wall_sizer.par
type page = Wall_sizer.page
type len = Wall_sizer.len

type 'a t

val allocate : size -> measure t
val map : ('a -> 'b) -> 'a t -> 'b t
val box : 'a t -> 'a t
val draw : ('a -> image) -> 'a t -> 'a t
val ( *&* ) : 'a t -> 'b t -> ('a * 'b) t
val ( *&  ) : 'a t -> unit t -> 'a t
val (  &* ) : unit t -> 'b t -> 'b t
val pad : ?left:size -> ?right:size -> 'a t -> 'a t

val ideal_size : _ t -> len * len
val minimal_size : _ t -> len * len
val render : page -> 'a t -> 'a * image
