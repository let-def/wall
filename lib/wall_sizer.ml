module Len = struct
  type t = float (* >= 0.0 *)
  let empty = 0.0
  let add = (+.)
  let add3 a b c = a +. b +. c
  let sub = (-.)
  let max (a : t) (b : t) = if a >= b then a else b
  let min (a : t) (b : t) = if a <= b then a else b
  let max3 a b c = max a (max b c)
  let min3 a b c = min a (min b c)
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

  let eval2 (physical : float) d0 d1 =
    let total = d0.fixed +. d1.fixed in
    if total >= physical then (
      (* Shrink *)
      let shrink_total = d0.shrink +. d1.shrink in
      let shrink_by = Len.min shrink_total (total -. physical) in
      if shrink_by > 0.0 then
        let f = shrink_by /. shrink_total in
        (d0.fixed -. d0.shrink *. f, d1.fixed -. d1.shrink *. f)
      else
        (d0.fixed, d1.fixed)
    ) else (
      let available = physical -. total in
      let stretch_total = d0.stretch +. d1.stretch in
      if stretch_total >= available then
        let f = available /. stretch_total in
        (* No need to fill *)
        (d0.fixed +. d0.stretch *. f,
         d1.fixed +. d1.stretch *. f)
      else
        (* Fill *)
        let fill_factor = d0.fill +. d1.fill in
        if fill_factor > 0.0 then
          let available = available -. stretch_total in
          let f = available /. fill_factor in
          (d0.fixed +. d0.stretch +. d0.fill *. f,
           d1.fixed +. d1.stretch +. d1.fill *. f)
        else
          (* Can't fill *)
          (d0.fixed +. d0.stretch,
           d1.fixed +. d1.stretch)
    )

  let eval3 (physical : float) d0 d1 d2 =
    let total = d0.fixed +. d1.fixed +. d2.fixed in
    if total >= physical then (
      (* Shrink *)
      let shrink_total = d0.shrink +. d1.shrink +. d2.shrink in
      let shrink_by = Len.min shrink_total (total -. physical) in
      if shrink_by > 0.0 then
        let f = shrink_by /. shrink_total in
        (d0.fixed -. d0.shrink *. f,
         d1.fixed -. d1.shrink *. f,
         d2.fixed -. d2.shrink *. f)
      else
        (d0.fixed, d1.fixed, d2.fixed)
    ) else (
      let available = physical -. total in
      let stretch_total = d0.stretch +. d1.stretch +. d2.stretch in
      if stretch_total >= available then
        let f = available /. stretch_total in
        (* No need to fill *)
        (d0.fixed +. d0.stretch *. f,
         d1.fixed +. d1.stretch *. f,
         d2.fixed +. d2.stretch *. f)
      else
        let fill_factor = d0.fill +. d1.fill +. d2.fill in
        if fill_factor > 0.0 then
          (* Fill *)
          let available = available -. stretch_total in
          let f = available /. fill_factor in
          (d0.fixed +. d0.stretch +. d0.fill *. f,
           d1.fixed +. d1.stretch +. d1.fill *. f,
           d2.fixed +. d2.stretch +. d2.fill *. f)
        else
          (* Can't fill *)
          (d0.fixed +. d0.stretch,
           d1.fixed +. d1.stretch,
           d2.fixed +. d2.stretch)
    )

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

type len = Len.t

type rect = {
  x : float;
  y : float;
  w : float;
  h : float;
}

type line = {
  x : float;
  y : float;
  h : float;
  w : float;
  d : float;
}

type par =
  | Simple of line
  | Complex of { top : line; body : rect; bot : line }

type dim = Dim.t

type 'a spec =
  | Rect   : { width : dim; height : dim } -> rect spec
  | Line   : { width : dim; height : dim; depth : dim; min_height : dim } -> line spec
  | Par_I  : line spec -> par spec
  | Par_LF : { top: line spec; body: rect spec; bot: line spec } -> par spec

let dim ?(shrink=Len.empty) ?(stretch=Len.empty) ?(fill=0.0) fixed =
  { Dim. fixed; shrink; stretch; fill }

let ideal_rect (Rect { width; height }) =
  (Dim.ideal width, Dim.ideal height)

let ideal_line (Line { width; height; depth }) =
  (Dim.ideal width, Len.add (Dim.ideal height) (Dim.ideal depth))

let ideal_size : type a. a spec -> len * len = function
  | Rect _ as t -> ideal_rect t
  | Line _ as t -> ideal_line t
  | Par_I t -> ideal_line t
  | Par_LF { top; body; bot } ->
    let w1, h1 = ideal_line top in
    let w2, h2 = ideal_rect body in
    let w3, h3 = ideal_line bot in
    (Len.max3 w1 w2 w3, Len.add3 h1 h2 h3)

let minimal_rect (Rect { width; height }) =
  (Dim.minimal width, Dim.minimal height)

let minimal_line (Line { width; height; depth }) =
  (Dim.minimal width, Len.add (Dim.minimal height) (Dim.minimal depth))

let minimal_size : type a. a spec -> len * len = function
  | Rect _ as t -> minimal_rect t
  | Line _ as t -> minimal_line t
  | Par_I t -> minimal_line t
  | Par_LF { top; body; bot } ->
    let w1, h1 = minimal_line top in
    let w2, h2 = minimal_rect body in
    let w3, h3 = minimal_line bot in
    (Len.max3 w1 w2 w3, Len.add3 h1 h2 h3)

let rect_empty = Rect { width = Dim.empty; height = Dim.empty }

module Line = struct
  let empty = Line { width = Dim.empty; height = Dim.empty; depth = Dim.empty;
                     min_height = Dim.empty }

  let spec ~width ~height ~depth =
    Line { width; height; depth; min_height = Dim.empty }

  let concat (Line l1) (Line l2) =
    Line { width  = Dim.add l1.width  l2.width;
           height = Dim.max l1.height l2.height;
           depth  = Dim.max l1.depth  l2.depth;
           min_height = Dim.max l1.min_height l2.min_height }

  let concat' (Line l1) (Line l2) base =
    let w1, w2 = Dim.eval2 base.w l1.width l2.width in
    ({base with w = w1}, {base with x = base.x +. w1; w = w2 })

  let width (Line l) = l.width

  let total_height (Line l) =
    Dim.max l.min_height (Dim.add l.height l.depth)

  let split h (Line l) =
    Dim.eval2 h l.height l.depth

  let rect spec =
    Rect { width = width spec; height = total_height spec }

  let rect' spec ({ x; y; w; h } : rect) =
    let h, d = split h spec in
    { x; y = y +. h; w; h; d }

  let par line =
    Par_I line

  let par' _line spec =
    match spec with
    | Simple line -> line
    | Complex { top; _ } ->
      (* Warn: incorrect use of API?
           Paragraph should have been simplified at this point *)
      top

end

module Par = struct
  type gravity = [`TOP | `BOTTOM]

  let lf : par spec =
    Par_LF { top = Line.empty; body = rect_empty; bot = Line.empty; }

  let empty : par spec =
    Par_I Line.empty

  let rect = function
    | Par_I line -> Line.rect line
    | Par_LF { top; body = Rect body; bot } ->
      let width = Dim.max3
          (Line.width top) body.width (Line.width bot)
      and height = Dim.add3
          (Line.total_height top) body.height (Line.total_height bot)
      in
      Rect { width; height }

  let rect' spec rect =
    match spec with
    | Par_I line -> Simple (Line.rect' line rect)
    | Par_LF { top; body = Rect body; bot } ->
      let { x; y; w; h } : rect = rect in
      let top_t, body, bot_t = Dim.eval3 h
          (Line.total_height top) body.height (Line.total_height bot)
      in
      let top_h, top_d = Line.split top_t top in
      let bot_h, bot_d = Line.split bot_t bot in
      let top_y  = y +. top_h in
      let body_y = y +. top_t in
      let bot_y  = body_y +. body +. bot_h in
      Complex {
        top  = { x; y = top_y;  w; h = top_h; d = top_d };
        body = { x; y = body_y; w; h = body };
        bot  = { x; y = bot_y;  w; h = bot_h; d = bot_d };
      }

  let line gravity = function
    | Par_I line -> line
    | Par_LF { top; body = Rect body; bot } ->
      let Line topl = top and Line botl = bot in
      let width =
        Dim.max3 (Line.width top) body.width (Line.width bot)
      and min_height =
        Dim.add3 topl.min_height body.height botl.min_height
      in
      match gravity with
      | `TOP ->
        Line { width; height = topl.height; min_height;
               depth = Dim.add3 topl.depth body.height (Line.total_height bot);
             }
      | `BOTTOM ->
        Line { width; depth = botl.depth; min_height;
               height = Dim.add3 (Line.total_height top) body.height botl.height }

  let line' gravity spec line =
    match spec with
    | Par_I _ -> Simple line
    | Par_LF { top; body = Rect body; bot } ->
      let { x; y; w; h; d } = line in
      match gravity with
      | `TOP ->
        (* FIXME: min_height ignored *)
        let Line topl = top in
        let top_d, body, bot_t = Dim.eval3 d
            topl.depth body.height (Line.total_height bot)
        in
        let bot_h, bot_d = Line.split bot_t bot in
        let body_y = y +. top_d in
        Complex {
          top  = { x; y; w; h; d = top_d };
          body = { x; y = body_y; w; h = body };
          bot  = { x; y = body_y +. body +. bot_h;
                   w; h = bot_h; d = bot_d };
        }
      | `BOTTOM ->
        let Line botl = bot in
        let top_t, body, bot_h = Dim.eval3 h
            (Line.total_height top) body.height botl.depth
        in
        let top_h, top_d = Line.split top_t top in
        let body_y = y -. body -. bot_h in
        Complex {
          top  = { x; y = body_y -. top_d; w; h = top_h; d = top_d };
          body = { x; y = body_y; w; h = body };
          bot  = { x; y; w; h = bot_h; d };
        }

  let get_complex top body bot = function
    | Complex _ as t -> t
    | Simple { x; y; w; h; d } ->
      (* Warn: incorrect use of API?
         Paragraph should have be complex at this point.
         Turn line into rectangle and solve as a rectangle *)
      let x = { x; y = y -. h; w; h = h +. d } in
      rect' (Par_LF { top; body; bot }) x

  let get_complex' top1 (Rect body1) midw midh (Rect body2) bot2 = function
    | Complex _ as t -> t
    | Simple _ as t ->
      (* Warn: incorrect use of API?
         Paragraph should have be complex at this point.
         Turn line into rectangle and solve as a rectangle *)
      let body = Rect {
          width = Dim.max3 body1.width midw body2.width;
          height = Dim.add3 body1.height midh body2.height;
        }
      in
      get_complex top1 body bot2 t

  let concat spec1 spec2 =
    match spec1, spec2 with
    | Par_I line1, Par_I line2 ->
      Par_I (Line.concat line1 line2)
    | Par_I line1, Par_LF { top; body; bot } ->
      Par_LF { top = Line.concat line1 top; body; bot }
    | Par_LF { top; body; bot }, Par_I line2 ->
      Par_LF { top; body; bot = Line.concat bot line2 }
    | Par_LF { top;        body = Rect r1; bot = mid1 },
      Par_LF { top = mid2; body = Rect r2; bot        } ->
      let mid = Line.concat mid1 mid2 in
      let lw = Line.width mid in
      let lh = Line.total_height mid in
      Par_LF { top; bot;
               body = Rect { width = Dim.max3 r1.width lw r2.width;
                             height = Dim.add3 r1.height lh r2.height } }

  let concat' spec1 spec2 par =
    match spec1, spec2 with
    | Par_I l1, Par_I l2 ->
      let line = match par with
        | Simple line -> line
        | Complex { top = line; _ } ->
          (* Warn: incorrect use of API?
             Paragraph should have been simplified at this point *)
          line
      in
      let l1, l2 = Line.concat' l1 l2 line in
      (Simple l1, Simple l2)
    | Par_I l1, Par_LF { top; body; bot }  ->
      let top' = Line.concat l1 top in
      begin match get_complex top' body bot par with
        | Simple _ -> assert false
        | Complex base ->
          let l1, top = Line.concat' l1 top base.top in
          (Simple l1, Complex {base with top})
      end
    | Par_LF { top; body; bot }, Par_I l2  ->
      let bot' = Line.concat bot l2 in
      begin match get_complex top body bot' par with
        | Simple _ -> assert false
        | Complex base ->
          let bot, l2 = Line.concat' bot l2 base.bot in
          (Complex {base with bot}, Simple l2)
      end
    | Par_LF ({ body = Rect b1; _} as lf1),
      Par_LF ({ body = Rect b2; _} as lf2) ->
      let mid = Line.concat lf1.bot lf2.top in
      let midw = Line.width mid in
      let midh = Line.total_height mid in
      begin match
          get_complex' lf1.top lf1.body midw midh lf2.body lf2.bot par
        with
        | Simple _ -> assert false
        | Complex base ->
          let h = base.body.h in
          let b1_h, midh, b2_h = Dim.eval3 h b1.height midh b2.height in
          let y1 = base.body.y +. b1_h in
          let mid' = Line.rect' mid {base.body with h = midh; y = y1} in
          let bot, top = Line.concat' lf1.bot lf2.top mid' in
          (Complex { base with body = { base.body with h = b1_h}; bot },
           Complex { base with top; body = { base.body with h = b2_h; y = y1 +. midh} })
      end

  let prepend line par =
    concat (Line.par line) par

  let prepend' line par spec =
    let p1, p2 = concat' (Line.par line) par spec in
    (Line.par' line p1, p2)

  let append par line =
      concat par (Line.par line)

  let append' par line spec =
    let p1, p2 = concat' par (Line.par line) spec in
    (p1, Line.par' line p2)
end

module Rect = struct

  let spec ~width ~height = Rect { width; height }

  type gravity = [
    | `TOP
    | `ABOVE
    | `CENTER
    | `BELOW
    | `BOTTOM
  ]

  let line gravity (Rect r) =
    let width      = r.width   in
    let height     = Dim.empty in
    let depth      = Dim.empty in
    let min_height = Dim.empty in
    match gravity with
    | `ABOVE ->
      let height = r.height in
      Line { width; height; depth; min_height }
    | `BELOW ->
      let depth = r.height in
      Line { width; height; depth; min_height }
    | `TOP | `CENTER | `BOTTOM ->
      let min_height = r.height in
      Line { width; height; depth; min_height }

  let line' gravity (Rect r) {x; y; w; h; d} =
    match gravity with
    | `ABOVE ->
      let h = Dim.eval h Dim.empty r.height Dim.empty in
      { x; y = y -. h; w; h }
    | `BELOW ->
      let d = Dim.eval d Dim.empty r.height Dim.empty in
      { x; y; w; h = d }
    | `TOP | `CENTER | `BOTTOM as case ->
      let h' = Dim.eval (h+.d) Dim.empty r.height Dim.empty in
      let y = match case with
        | `TOP    -> y -. h
        | `CENTER -> y -. h' /. 2.0
        | `BOTTOM -> y +. d -. h'
      in
      { x; y; w; h = h' }

  let par gravity spec =
    Line.par (line gravity spec)

  let par' gravity spec par =
    let spec' = line gravity spec in
    line' gravity spec (Line.par' spec' par)

  (* Horizontal concatenation *)
  let concat (Rect r1) (Rect r2) =
    Rect { width = Dim.max r1.width r2.width;
           height = Dim.max r1.height r2.height }

  let concat' (Rect r1) (Rect r2) (r : rect) =
    let w1, w2 = Dim.eval2 r.w r1.width r2.width in
    ({r with w = w1}, {r with x = r.x +. w1; w = w2})
end

module Op = struct
  type ('a, 'b) unary = {
    fwd1 : 'a spec -> 'b spec;
    bkd1 : 'a spec -> 'b -> 'a;
  }

  let apply_unary unary spec = unary.fwd1 spec
  let solve_unary unary spec sol = unary.bkd1 spec sol

  let id : 'a. ('a, 'a) unary = { fwd1 = (fun x -> x); bkd1 = (fun _ x -> x) }
  let compose (a : ('a, 'b) unary) (b : ('b, 'c) unary) : ('a, 'c) unary =
    {
      fwd1 = (fun spec -> b.fwd1 (a.fwd1 spec));
      bkd1 = (fun spec sol -> a.bkd1 spec (b.bkd1 (a.fwd1 spec) sol));
    }

  type ('a, 'b, 'c) binary = {
    fwd2 : 'a spec -> 'b spec -> 'c spec;
    bkd2 : 'a spec -> 'b spec -> 'c -> 'a * 'b;
  }

  let apply_binary binary speca specb =
    binary.fwd2 speca specb
  let solve_binary binary speca specb sol =
    binary.bkd2 speca specb sol
end

module Ops : sig
  val line_of_rect : Rect.gravity -> (rect, line) Op.unary
  val par_of_rect  : Rect.gravity -> (rect, par) Op.unary

  val line_concat  : (line, line, line) Op.binary
  val rect_of_line : (line, rect) Op.unary
  val par_of_line  : (line, par ) Op.unary

  val rect_of_par  : (par, rect) Op.unary
  val line_of_par  : Par.gravity -> (par, line) Op.unary
  val par_concat   : (par , par , par) Op.binary
  val par_prepend  : (line, par , par) Op.binary
  val par_append   : (par , line, par) Op.binary

  val concat : ('a, 'a, 'a) Op.binary
end = struct
  let line_of_rect gravity =
    { Op. fwd1 = Rect.line gravity; bkd1 = Rect.line' gravity }

  let par_of_rect gravity =
    { Op. fwd1 = Rect.par gravity; bkd1 = Rect.par' gravity }

  let line_concat =
    { Op. fwd2 = Line.concat; bkd2 = Line.concat' }

  let rect_of_line =
    { Op. fwd1 = Line.rect; bkd1 = Line.rect' }

  let par_of_line =
    { Op. fwd1 = Line.par; bkd1 = Line.par' }

  let rect_of_par =
    { Op. fwd1 = Par.rect; bkd1 = Par.rect' }

  let line_of_par gravity =
    { Op. fwd1 = Par.line gravity; bkd1 = Par.line' gravity }

  let par_concat =
    { Op. fwd2 = Par.concat; bkd2 = Par.concat' }
  let par_prepend =
    { Op. fwd2 = Par.prepend; bkd2 = Par.prepend' }
  let par_append =
    { Op. fwd2 = Par.append; bkd2 = Par.append' }

  let concat_fwd (type a) : a spec -> a spec -> a spec = fun a b ->
      match a with
      | Rect _ -> Rect.concat a b
      | Line _ -> Line.concat a b
      | Par_I _ -> Par.concat a b
      | Par_LF _ -> Par.concat a b

  let concat_bkd (type a) : a spec -> a spec -> a -> a * a = fun a b ->
      match a with
      | Rect _ -> Rect.concat' a b
      | Line _ -> Line.concat' a b
      | Par_I _ -> Par.concat' a b
      | Par_LF _ -> Par.concat' a b

  let concat = { Op. fwd2 = concat_fwd; bkd2 = concat_bkd }
end
