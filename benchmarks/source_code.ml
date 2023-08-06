open Wall
module I = Image
module Text = Wall_text

type command =
  { color : Color.t
  ; text : string
  ; x : float
  ; y : float
  }

type t =
  { font : Text.Font.t
  ; commands : command list
  ; text_height : float
  ; text_width : float
  ; kind : string
  }

type state = t list

let init ~placement =
  let font =
    let tt =  Bench.load_font "./RobotoMono-Light.ttf" in
    Text.Font.make ~size:18.0 ~placement tt
  in
  let metrics = Text.Font.font_metrics font in
  let source_code =
    In_channel.open_text "../lib/wall.ml"
    |> In_channel.input_all
    |> String.split_on_char '\n'
  in
  let gap = metrics.ascent +. metrics.descent +. metrics.line_gap in
  let gap = gap *. 1.5 in
  let (text_height, text_width), commands =
    List.fold_left_map
      (fun (y, width) line ->
        let cmd = { color = Color.white; text = line; x = gap *. 2.0; y } in
        let measure = Text.Font.text_measure font line in
        let y = y +. gap in
        let width = Float.max width measure.width in
        (y, width), cmd)
      (0.0, gap *. 2.0)
      source_code
  in
  let kind =
    match placement with
    | `Aligned -> "ALIGNED"
    | `Subpixel -> "SUBPIXEL"
  in
  { font; commands; text_height; text_width; kind }
;;

let init _ctx = [ init ~placement:`Aligned; init ~placement:`Subpixel ]

let paint_text ?(valign = `TOP) ?(halign = `LEFT) ~x ~y ~color ~font s =
  I.paint (Paint.color color) (Text.simple_text font ~valign ~halign ~x ~y s)
;;

let rotate_around ~x ~y ~a = 
  Transform.translation ~x ~y 
    |> Transform.rotate a 
    |> Transform.translate ~x:(-. x) ~y:(-. y)
;;

let frame state ~width ~height ~elapsed_seconds =
  let _x_offset, regions =
    List.fold_left_map
      (fun x_offset { font; commands; text_height; text_width; kind } ->
        let matrix =
          let t = (Float.cos (elapsed_seconds /. 5.0) +. 1.0) /. 2.0 in
          let t = if t >= 0.8 then 1.0 else t *. (1.0 /. 0.8) in
          Transform.translation ~x:x_offset ~y:(-.(t *. (text_height -. height)))
        in
        let image =
          let code =
            List.map
              (fun { color; text; x; y } -> paint_text ~x ~y ~color ~font text)
              commands
            |> I.seq
            |> I.transform matrix
          in
          let label = 
            paint_text ~x:(x_offset +. 5.0) ~y:(-. 5.0) ~color:Color.white ~valign:`BOTTOM ~font kind 
            |> I.transform (rotate_around ~x:x_offset ~y:0.0 ~a:(Float.pi /. 2.0)) 
          in 
          I.stack code label
        in
        x_offset +. text_width, image)
      0.0
      state
  in
  I.seq regions
;;

let name = "source-code"
let clear_color = Color.black
