open Wall

let background =
  Paint.linear_gradient ~sx:0.0 ~sy:0.0 ~ex:1024.0 ~ey:0.0
     ~inner:Color.white ~outer:(Color.with_a Color.blue 0.5)

let mediabox = Path.make @@ fun t ->
  Path.rect t ~x:0.0 ~y:0.0 ~w:1024.0 ~h:768.0

let load_font name =
  let ic = open_in_bin name in
  let dim = in_channel_length ic in
  let fd = Unix.descr_of_in_channel ic in
  let buffer =
    Unix.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false [|dim|]
    |> Bigarray.array1_of_genarray
  in
  let offset = List.hd (Stb_truetype.enum buffer) in
  match Stb_truetype.init buffer offset with
  | None -> assert false
  | Some font -> font

let font_sans = load_font "Roboto-Regular.ttf"
let font_mono = load_font "RobotoMono-Regular.ttf"

let nyan_cat =
  match Stb_image.load "nyan_cat.png" with
  | Result.Error (`Msg x) ->
    Printf.ksprintf prerr_endline "loading nyan_cat: %s" x;
    failwith "No image"
  | Result.Ok img -> Wall_texture.from_image ~name:"nyan" img
