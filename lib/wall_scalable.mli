open Wall
open Wall_sizer

type 'a t = Scalable of 'a spec * ('a -> image)

val allocate : 'a spec -> 'a t

val apply : ('a, 'b) Op.unary -> 'a t -> 'b t
val join : ('a, 'b, 'c) Op.binary -> 'a t -> 'b t -> 'c t

val ( *&* ) : 'a t -> 'a t -> 'a t
val pad : ?left:'a spec -> ?right:'a spec -> 'a t -> 'a t

val draw : ('a -> image) -> 'a t -> 'a t

val spec : 'a t -> 'a spec
val render : 'a -> 'a t -> image
