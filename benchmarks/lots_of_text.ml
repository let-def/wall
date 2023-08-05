open Wall
module I = Image
module Text = Wall_text

type command =
  { color : Color.t
  ; text : string
  ; leave : bool
  ; x : float
  ; y : float
  }

type state =
  { font : Text.Font.t
  ; commands : command list
  }

let init _ctx =
  let font =
    let tt = Lazy.force Bench.font_sans in
    Text.Font.make ~size:30.0 ~placement:`Subpixel tt
  in
  let commands = ref [] in
  let push i = commands := i :: !commands in
  for x = 0 to 100 do
    let x = Float.of_int x *. 100.0 in
    for y = 0 to 100 do
      let y = Float.of_int y *. 50.0 in
      let c1, c2 =
        ( Color.v
            (Random.float 1.0)
            (Random.float 1.0)
            (Random.float 1.0)
            (Random.float 1.0)
        , Color.v
            (Random.float 1.0)
            (Random.float 1.0)
            (Random.float 1.0)
            (Random.float 1.0) )
      in
      push { color = c1; text = "hello"; leave = Random.bool (); x; y };
      push { color = c2; text = "world"; leave = Random.bool (); x; y }
    done
  done;
  { font; commands = !commands }
;;

let paint_text ~x ~y ~color ~font s =
  I.paint (Paint.color color) (Text.simple_text font ~valign:`TOP ~halign:`LEFT ~x ~y s)
;;

let frame { font; commands } ~width ~height ~elapsed_seconds =
  let matrix =
    let scale =
      Transform.scale
        ~sx:((Float.sin elapsed_seconds +. 1.25) /. 2.0)
        ~sy:((Float.sin elapsed_seconds +. 1.25) /. 2.0)
    in
    let forward =
      Transform.translation
        ~x:((-.width /. 2.0) -. 5000.0)
        ~y:((-.height /. 2.0) -. 2500.0)
    in
    let backward = Transform.translation ~x:(width /. 2.0) ~y:(height /. 2.0) in
    Transform.compose forward (Transform.compose scale backward)
  in
  let should_disappear = Float.to_int (elapsed_seconds /. 2.0) mod 2 = 0 in
  List.filter_map
    (fun { color; text; leave; x; y } ->
      if should_disappear && leave
      then None
      else Some (paint_text ~x ~y ~color ~font text))
    commands
  |> I.seq
  |> I.transform matrix
;;

let name = "lots-of-text"
