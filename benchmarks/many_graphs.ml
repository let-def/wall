open Wall
module I = Image
module P = Path

type state = unit

let init _ctx = ()

let draw_graph x y w h t =
  let samples =
    [| (1.0 +. sin ((t *. 1.2345) +. (cos (t *. 0.33457) *. 0.44))) *. 0.5
     ; (1.0 +. sin ((t *. 0.68363) +. (cos (t *. 1.3) *. 1.55))) *. 0.5
     ; (1.0 +. sin ((t *. 1.1642) +. (cos (t *. 0.33457) *. 1.24))) *. 0.5
     ; (1.0 +. sin ((t *. 0.56345) +. (cos (t *. 1.63) *. 0.14))) *. 0.5
     ; (1.0 +. sin ((t *. 1.6245) +. (cos (t *. 0.254) *. 0.3))) *. 0.5
     ; (1.0 +. sin ((t *. 0.345) +. (cos (t *. 0.03) *. 0.6))) *. 0.5
    |]
  in
  let dx = w /. 5.0 in
  let sx i = x +. (float i *. dx) in
  let sy i = y +. (h *. samples.(i) *. 0.8) in
  I.seq
    [ (* Graph background *)
      I.paint
        (Paint.linear_gradient
           ~sx:x
           ~sy:y
           ~ex:x
           ~ey:(y +. h)
           ~inner:(Color.v 0.00 0.60 0.75 0.00)
           ~outer:(Color.v 0.00 0.60 0.75 0.25))
        (I.fill_path
         @@ fun t ->
         P.move_to t ~x:(sx 0) ~y:(sy 0);
         for i = 1 to 5 do
           P.bezier_to
             t
             ~c1x:(sx (i - 1) +. (dx *. 0.5))
             ~c1y:(sy (i - 1))
             ~c2x:(sx i -. (dx *. 0.5))
             ~c2y:(sy i)
             ~x:(sx i)
             ~y:(sy i)
         done;
         P.line_to t ~x:(x +. w) ~y:(y +. h);
         P.line_to t ~x ~y:(y +. h))
    ; (* Graph line *)
      I.paint
        (Paint.color (Color.v 0.0 0.0 0.0 0.125))
        (I.stroke_path Outline.{ default with stroke_width = 3.0 }
         @@ fun t ->
         P.move_to t ~x:(sx 0) ~y:(sy 0 +. 2.0);
         for i = 1 to 5 do
           P.bezier_to
             t
             ~c1x:(sx (i - 1) +. (dx *. 0.5))
             ~c1y:(sy (i - 1) +. 2.0)
             ~c2x:(sx i -. (dx *. 0.5))
             ~c2y:(sy i +. 2.0)
             ~x:(sx i)
             ~y:(sy i +. 2.0)
         done)
    ; I.paint
        (Paint.color (Color.v 0.0 0.60 0.75 1.0))
        (I.stroke_path Outline.{ default with stroke_width = 3.0 }
         @@ fun t ->
         P.move_to t ~x:(sx 0) ~y:(sy 0);
         for i = 1 to 5 do
           P.bezier_to
             t
             ~c1x:(sx (i - 1) +. (dx *. 0.5))
             ~c1y:(sy (i - 1))
             ~c2x:(sx i -. (dx *. 0.5))
             ~c2y:(sy i)
             ~x:(sx i)
             ~y:(sy i)
         done)
    ; (* Graph sample pos *)
      (let node = ref I.empty in
       for i = 0 to 5 do
         node
           := I.stack
                !node
                (I.paint
                   (Paint.radial_gradient
                      ~cx:(sx i)
                      ~cy:(sy i +. 2.0)
                      ~inr:3.0
                      ~outr:8.0
                      ~inner:(Color.v 0.0 0.0 0.0 0.125)
                      ~outer:(Color.v 0.0 0.0 0.0 0.0))
                   (I.fill_path
                    @@ fun t ->
                    P.rect t ~x:(sx i -. 10.0) ~y:(sy i -. 10.0 +. 2.0) ~w:20.0 ~h:20.0))
       done;
       !node)
    ; I.paint
        (Paint.color (Color.v 0.0 0.6 0.75 1.0))
        (I.fill_path
         @@ fun t ->
         for i = 0 to 5 do
           P.circle t ~cx:(sx i) ~cy:(sy i) ~r:4.0
         done)
    ; I.paint
        (Paint.color (Color.v 0.8 0.8 0.8 1.0))
        (I.fill_path
         @@ fun t ->
         for i = 0 to 5 do
           P.circle t ~cx:(sx i) ~cy:(sy i) ~r:2.0
         done)
    ]
;;

let many_graphs ~width:w ~height:h t =
  let node = ref I.empty in
  let push n = node := I.stack !node n in
  for i = 0 to 500 do
    push @@ draw_graph 0.0 0.0 w h (t +. float i)
  done;
  !node
;;

let many_graphs_cached = ref None

let many_graphs ~width ~height time =
  match !many_graphs_cached with
  | Some (w, h, t, cached)
    when Float.equal w width && Float.equal h height && Float.equal t time -> cached
  | _ ->
    let cached = many_graphs ~width ~height time in
    many_graphs_cached := Some (width, height, time, cached);
    cached
;;

let frame () ~width ~height ~elapsed_seconds =
  I.stack
    (many_graphs ~width ~height 0.0)
    (draw_graph 0.0 0.0 width height elapsed_seconds)
;;

let name = "many-graphs"
let clear_color = Color.black
