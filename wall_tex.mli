type t
val make : name:string -> t
val release : t -> unit
val tex : t -> int

val flip_image : 'a Stb_image.t -> unit
val from_image : name:string -> 'a Stb_image.t -> t
val load_image :
  ?float:bool -> ?alpha:bool -> ?flip:bool -> ?name:string -> string ->
  (t, [`Msg of string]) Result.result
