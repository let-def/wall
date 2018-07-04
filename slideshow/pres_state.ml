open Wall

let background =
  Paint.linear_gradient ~sx:0.0 ~sy:0.0 ~ex:1024.0 ~ey:0.0
     ~inner:Color.white ~outer:(Color.with_a Color.blue 0.5)

let mediabox = Path.make (fun ctx -> Path.rect ctx 0.0 0.0 1024.0 768.0)

let load_font name =
  let ic = open_in_bin name in
  let dim = in_channel_length ic in
  let fd = Unix.descr_of_in_channel ic in
  let buffer =
    Bigarray.Array1.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false dim
  in
  let offset = List.hd (Stb_truetype.enum buffer) in
  match Stb_truetype.init buffer offset with
  | None -> assert false
  | Some font -> font

let font_sans = load_font "Roboto-Regular.ttf"
let font_mono = load_font "RobotoMono-Regular.ttf"
