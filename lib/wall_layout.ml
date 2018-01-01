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

module Len = struct
  type t = float (* >= 0.0 *)
  let empty = 0.0
  let add = (+.)
  let sub = (-.)
  let max (a : t) (b : t) = if a >= b then a else b
  let min (a : t) (b : t) = if a <= b then a else b
  let (<=) : t -> t -> bool = (<=)
  let (>=) : t -> t -> bool = (>=)
  let (> ) : t -> t -> bool = (> )
  let (< ) : t -> t -> bool = (< )
  let (= ) : t -> t -> bool = (= )
  let (<>) : t -> t -> bool = (<>)
end

module Dim = struct
  type t = {
    fixed: Len.t;
    shrink: Len.t;
    stretch: Len.t;
    fill: float;
  }

  let add a b = {
    fixed = Len.add a.fixed b.fixed;
    shrink = Len.add a.shrink b.shrink;
    stretch = Len.add a.stretch b.stretch;
    fill = a.fill +. b.fill;
  }

  let max a b =
    let fixed = Len.max a.fixed b.fixed in
    let shrink =
      let a' = Len.sub a.fixed a.shrink and b' = Len.sub b.fixed b.shrink in
      Len.sub fixed (Len.max a' b')
    in
    let stretch =
      let a' = Len.add a.fixed a.stretch and b' = Len.add b.fixed b.stretch in
      Len.sub (Len.max a' b') fixed
    in
    let fill = Len.max a.fill b.fill in
    { fixed; shrink; stretch; fill }

  let empty = { fixed = Len.empty; shrink = Len.empty; stretch = Len.empty; fill = 0.0 }

  let add3 a b c = add a (add b c)

  let max3 a b c = max a (max b c)

  let eval (physical : float) d0 dim d1 =
    let total = d0.fixed +. dim.fixed +. d1.fixed in
    if total >= physical then (
      (* Shrink *)
      let shrink_total = d0.shrink +. dim.shrink +. d1.shrink in
      let shrink_by = Len.min shrink_total (total -. physical) in
      if dim.shrink > 0.0 && shrink_by > 0.0
      then dim.fixed -. shrink_by *. dim.shrink /. shrink_total
      else dim.fixed
    ) else (
      let available = physical -. total in
      let stretch_total = d0.stretch +. dim.stretch +. d1.stretch in
      if stretch_total >= available then
        (* No need to fill *)
        dim.fixed +. dim.stretch *. available /. stretch_total
      else if dim.fill > 0.0 then
        (* Fill *)
        let fill_factor = d0.fill +. dim.fill +. d1.fill in
        let available = available -. stretch_total in
        dim.fixed +. dim.stretch +. available *. dim.fill /. fill_factor
      else
        (* Can't fill *)
        dim.fixed +. dim.stretch
    )

  let ideal d = d.fixed

  let minimal d = d.fixed -. d.shrink
end

module Box = struct
  type t = {
    width: Dim.t;
    height: Dim.t;
    depth: Dim.t;
  }

  let empty = { width = Dim.empty; height = Dim.empty; depth = Dim.empty }

  let join a b = {
    width  = Dim.add a.width  b.width;
    height = Dim.max a.height b.height;
    depth  = Dim.max a.depth  b.depth;
  }

  let stack a b = {
    width  = Dim.max a.width b.width;
    height = a.height;
    depth  = Dim.add3 a.depth b.height b.depth;
  }

  let stack3 a b c =
    stack a (stack b c)

  let over a b = {
    width  = Dim.max a.width  b.width;
    height = Dim.max a.height b.height;
    depth  = Dim.max a.depth  b.depth;
  }

  let total_height b = Dim.add b.height b.depth
end

module Par = struct
  type t =
    | I of Box.t
    | LF of { top: Box.t; bdy: Box.t; bot: Box.t }

  let empty = I Box.empty

  let lf = LF { top = Box.empty; bdy = Box.empty; bot = Box.empty; }

  let add a b = match a, b with
    | I a, I b -> I (Box.join a b)
    | I a, LF b -> LF {b with top = Box.join a b.top}
    | LF a, I b -> LF {a with bot = Box.join a.bot b}
    | LF {top; bdy = l; bot = x},
      LF {top = y; bdy = r; bot} ->
      LF {top; bot; bdy = Box.stack3 l (Box.join x y) r}

  let box = function
    | I b -> b
    | LF { top; bdy; bot } -> Box.stack3 top bdy bot
end

module Page = struct
  type t =
    | Simple of line
    | Complex of line * rect * line

  let line x y w h d =
    { x; y; w; h; d }

  let rect x y w h = { x; y; w; h }

  let setup_line ~x ~y ~w ~h {Box. width; height; depth} =
    let d = Dim.eval h height    depth  Dim.empty
    and w = Dim.eval w Dim.empty width  Dim.empty
    and h = Dim.eval h Dim.empty height depth
    in line x (y +. h) w h d

  let setup ~x ~y ~w ~h = function
    | Par.I b -> Simple (setup_line ~x ~y ~w ~h b)
    | Par.LF { top; bdy; bot } ->
      let top_dim = Box.total_height top in
      let bdy_dim = Box.total_height bdy in
      let bot_dim = Box.total_height bot in
      let w = Dim.eval w Dim.empty (Box.stack3 top bdy bot).Box.width Dim.empty in
      let top_h = Dim.eval h Dim.empty top_dim (Dim.add bdy_dim bot_dim) in
      let bdy_h = Dim.eval h top_dim bdy_dim bot_dim in
      let bot_h = Dim.eval h (Dim.add top_dim bdy_dim) bot_dim Dim.empty in
      Complex (setup_line ~x ~y ~w ~h:top_h top,
               rect x (y +. top_h) w bdy_h,
               setup_line ~x ~y:(y +. top_h +. bdy_h) ~w ~h:bot_h bot)

  let get_line_rect (l : line) =
    rect l.x (l.y -. l.h) l.w (l.h +. l.d)

  let get_rect = function
    | Simple l -> get_line_rect l
    | Complex (l1, r, l2) ->
      let y1 = (l1.y -. l1.h) in
      let y2 = (l2.y +. l2.d) in
      rect r.x y1 r.w (y2 -. y1)

  let get_line (Simple line | Complex (line, _, _)) = line

  let get_complex t p1 p2 = match t with
    | Complex (l1,r,l2) -> (l1,r,l2)
    | Simple line ->
      let { x; y; w; h } = get_line_rect line in
      match setup ~x ~y ~w ~h (Par.add p1 p2) with
      | Simple _ -> assert false
      | Complex (l1,r,l2) -> (l1,r,l2)

  let split_line (base : line) b1 b2 =
    let w1 = Dim.eval base.w Dim.empty b1.Box.width b2.Box.width in
    let w2 = Dim.eval base.w b1.Box.width b2.Box.width Dim.empty in
    let line1 = line base.x base.y w1
        (Dim.eval base.h Dim.empty b1.Box.height Dim.empty)
        (Dim.eval base.d Dim.empty b1.Box.depth  Dim.empty)
    and line2 = line (base.x +. w1) base.y w2
        (Dim.eval base.h Dim.empty b2.Box.height Dim.empty)
        (Dim.eval base.d Dim.empty b2.Box.depth  Dim.empty)
    in
    line1, line2

  let split t p1 p2 =
    match p1, p2 with
    | Par.I b1, Par.I b2 ->
      let line1, line2 = split_line (get_line t) b1 b2 in
      (Simple line1, Simple line2)

    | Par.LF lf, Par.I b2 ->
      let (l1,r,l2) = get_complex t p1 p2 in
      let l2', l3 = split_line l2 lf.bot b2 in
      (Complex (l1,r,l2'), Simple l3)

    | Par.I b1, Par.LF lf ->
      let (l1,r,l2) = get_complex t p1 p2 in
      let l0, l1' = split_line l1 b1 lf.top in
      (Simple l0, Complex (l1',r,l2))

    | Par.LF lf1, Par.LF lf2 ->
      let (l0,r,l3) = get_complex t p1 p2 in
      let mid  = Box.join lf1.bot lf2.top in
      let dim0 = Box.total_height lf1.bdy in
      let dim1 = Box.total_height mid in
      let dim2 = Box.total_height lf2.bdy in
      let h0 = Dim.eval r.h Dim.empty dim0 (Dim.add dim1 dim2) in
      let h1 = Dim.eval r.h dim0 dim1 dim2 in
      let h2 = Dim.eval r.h (Dim.add dim0 dim1) dim2 Dim.empty in
      let line = setup_line ~x:r.x ~y:(r.y +. h0) ~w:r.w ~h:h1 mid in
      let l1, l2 = split_line line lf1.bot lf2.top in
      (Complex (l0,{r with h = h0},l1),
       Complex (l2,{r with y = r.y +. h0 +. h1; h = h2},l3))
end

type len = Len.t

type dim = Dim.t
let dim ?(shrink=Len.empty) ?(stretch=Len.empty) ?(fill=0.0) fixed =
  { Dim. fixed; shrink; stretch; fill }

type box = Box.t
let atom ~width ~height ~depth = { Box. width; height; depth }

type par = Par.t
let par box = Par.I box
let box = Par.box
let lf = Par.lf
let (--) = Par.add
let empty = Par.empty

let ideal_size par =
  let box = Par.box par in
  let height = Box.total_height box in
  (Dim.ideal box.Box.width, Dim.ideal height)

let minimal_size par =
  let box = Par.box par in
  let height = Box.total_height box in
  (Dim.minimal box.Box.width, Dim.minimal height)

type page = Page.t
let page ?(x=0.0) ?(y=0.0) ~width ~height par =
  Page.setup ~x ~y ~w:width ~h:height par
let split = Page.split
let page_line = Page.get_line
let page_rect = Page.get_rect
