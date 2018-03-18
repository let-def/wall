(*
   Copyright (c) 2015 Frédéric Bour <frederic.bour@lakaban.net>

   This software is provided 'as-is', without any express or implied
   warranty.  In no event will the authors be held liable for any damages
   arising from the use of this software.
   Permission is granted to anyone to use this software for any purpose,
   including commercial applications, and to alter it and redistribute it
   freely, subject to the following restrictions:
   1. The origin of this software must not be misrepresented; you must not
      claim that you wrote the original software. If you use this software
      in a product, an acknowledgment in the product documentation would be
      appreciated but is not required.
   2. Altered source versions must be plainly marked as such, and must not be
      misrepresented as being the original software.
   3. This notice may not be removed or altered from any source distribution.
*)

[@@@landmark "auto"]
module BA = Bigarray.Array1
let ba_empty = BA.create Bigarray.float32 Bigarray.c_layout 0

(* Algorithms from
   https://github.com/memononen/nanovg/blob/master/src/nanovg.c *)

let minf a b : float = if a < b then a else b
let maxf a b : float = if a >= b then a else b

module T = struct
  type winding = CW | CCW

  type path = {
    path_first : int;
    mutable path_count : int;
    mutable path_closed : bool;
    mutable path_winding: winding;
    mutable path_convex : bool;
    mutable path_nbevel : int;
  }

  let flag_corner      = 0x1
  let flag_left        = 0x2
  let flag_bevel       = 0x4
  let flag_innerbevel  = 0x8

  type fbuffer =
    (float, Bigarray.float32_elt, Bigarray.c_layout) BA.t

  type u8buffer =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) BA.t

  module T : sig
    type t = {
      mutable dist_tol : float;
      mutable tess_tol : float;
      mutable observed_tol : bool;
      mutable paths: path list;
      mutable point: int;
      mutable points: fbuffer;
      mutable points_flags: u8buffer;
      mutable points_aux: fbuffer;
    }

    val make : unit -> t
    val clear : t -> unit
    val count : t -> int

    val set_tol : t -> dist:float -> tess:float -> unit
    val dist_tol : t -> float
    val set_tess_tol : t -> float -> unit
    val tess_tol : t -> float

    val add_point : t -> float -> float -> int -> unit
    val set_last_flags : t -> int -> unit

    val reverse_points : t -> path -> unit
    val points_area : t -> path -> float

    val add_path : t -> unit
    val flush_paths : t -> path list

    val has_path : t -> bool
    val current_path : t -> path
    val observed_tol : t -> bool

    val last_x : t -> float
    val last_y : t -> float

    val get_x : t -> int -> float
    val get_y : t -> int -> float

    val get_flags : t -> int -> int
    val set_flags : t -> int -> int -> unit

    val prepare_aux : t -> unit

    val aux_set_d : t -> int -> float -> float -> float -> unit

    val aux_dx : t -> int -> float
    val aux_dy : t -> int -> float
    val aux_dlen : t -> int -> float
    val aux_dmx : t -> int -> float
    val aux_dmy : t -> int -> float
  end = struct

    type t = {
      mutable dist_tol : float;
      mutable tess_tol : float;
      mutable observed_tol : bool;
      mutable paths: path list;
      mutable point: int;
      mutable points: fbuffer;
      mutable points_flags: u8buffer;
      mutable points_aux: fbuffer;
    }


    let make () = {
      dist_tol = 0.01;
      tess_tol = 0.25;
      paths = [];
      point = 0;
      points = BA.create Bigarray.float32 Bigarray.c_layout 1024;
      points_flags = BA.create Bigarray.int8_unsigned Bigarray.c_layout 512;
      points_aux = ba_empty;
      observed_tol = false;
    }

    let clear t =
      t.observed_tol <- false;
      t.point <- 0;
      t.paths <- []

    let count t = t.point

    let set_tol t ~dist ~tess =
      t.dist_tol <- dist;
      t.tess_tol <- tess

    let dist_tol t = t.dist_tol

    let set_tess_tol t tol =
      t.tess_tol <- tol

    let tess_tol t =
      t.observed_tol <- true;
      t.tess_tol

    let observed_tol t = t.observed_tol

    let grow t =
      let d0 = BA.dim t.points_flags in
      let d = d0 * 3 / 2 in
      (*Printf.printf "grow: allocating %d points\n%!" d;*)
      let points = BA.create Bigarray.float32 Bigarray.c_layout (d * 2) in
      let points_flags = BA.create Bigarray.int8_unsigned Bigarray.c_layout d in
      BA.blit t.points (BA.sub points 0 (d0 * 2));
      BA.blit t.points_flags (BA.sub points_flags 0 d0);
      t.points <- points;
      t.points_flags <- points_flags

    let add_point t x y flags =
      if t.point >= BA.dim t.points_flags then
        (grow t; assert (t.point < BA.dim t.points_flags));
      let {point; points; points_flags} = t in
      points.{point * 2 + 0} <- x;
      points.{point * 2 + 1} <- y;
      (*Printf.printf "add_point: %d = (%f, %f)\n" point x y;*)
      points_flags.{point}   <- flags;
      t.point <- point + 1

    let set_last_flags t flags =
      t.points_flags.{t.point - 1} <- flags

    let reverse_points {points; points_flags} {path_first; path_count} =
      let last = path_first + path_count - 1 in
      for k = 0 to path_count / 2 - 1 do
        let i = path_first + k and j = last - k in
        let flags = points_flags.{i} in
        let x = points.{2 * i + 0} and y = points.{2 * i + 1} in
        points_flags.{i}   <- points_flags.{j};
        points.{2 * i + 0} <- points.{2 * j + 0};
        points.{2 * i + 1} <- points.{2 * j + 1};
        points_flags.{j}   <- flags;
        points.{2 * j + 0} <- x;
        points.{2 * j + 1} <- y;
      done

    let points_area {points} {path_first; path_count} =
      let ax = points.{2 * path_first + 0} and ay = points.{2 * path_first + 1} in
      let area = ref 0.0 in
      for k = path_first + 2 to path_first + path_count - 1 do
        let bx = points.{2 * k - 2} and by = points.{2 * k - 1} in
        let abx = bx -. ax and aby = by -. ay in
        let cx = points.{2 * k + 0} and cy = points.{2 * k + 1} in
        let acx = cx -. ax and acy = cy -. ay in
        area := !area +. (acx *. aby -. abx *. acy)
      done;
      (!area *. 0.5)

    let paths t = t.paths

    let freeze_path t = function
      | [] -> ()
      | p :: _ -> p.path_count <- t.point - p.path_first

    let add_path t =
      freeze_path t t.paths;
      let p = {
        path_first = t.point;
        path_count = -1;
        path_closed = false;
        path_winding = CCW;
        path_convex = false;
        path_nbevel = 0;
      } in
      t.paths <- p :: t.paths

    let has_path t = t.paths <> []

    let current_path t = match t.paths with
      | [] -> invalid_arg "Wall__geom.current_path"
      | p :: _ -> p

    let flush_paths t =
      let paths = t.paths in
      freeze_path t paths;
      (*t.paths <- [];*)
      paths

    let last_x {points; point} =
      if point > 0 then points.{2 * (point - 1) + 0} else 0.0

    let last_y {points; point} =
      if point > 0 then points.{2 * (point - 1) + 1} else 0.0

    let get_x {points} i = points.{2 * i + 0}
    let get_y {points} i = points.{2 * i + 1}

    let get_flags {points_flags} i = points_flags.{i}
    let set_flags {points_flags} i v = points_flags.{i} <- v

    let prepare_aux t =
      if BA.dim t.points_aux < t.point * 5 then
        let count = BA.dim t.points * 5 / 2 in
        (*Printf.printf "prepare_aux: allocating %d points\n%!" count;*)
        t.points_aux <- BA.create Bigarray.float32 Bigarray.c_layout count

    let aux_set_d {points_aux} pt dx dy dlen =
      (*Printf.printf "set_d: %f %f %f\n" dx dy dlen;*)
      points_aux.{5 * pt + 0} <- dx;
      points_aux.{5 * pt + 1} <- dy;
      points_aux.{5 * pt + 2} <- dlen

    let aux_dx   {points_aux} pt = points_aux.{5 * pt + 0}
    let aux_dy   {points_aux} pt = points_aux.{5 * pt + 1}
    let aux_dlen {points_aux} pt = points_aux.{5 * pt + 2}
    let aux_dmx  {points_aux} pt = points_aux.{5 * pt + 3}
    let aux_dmy  {points_aux} pt = points_aux.{5 * pt + 4}
  end

  type t = T.t
  let make = T.make

  type m_bounds = {
    mutable m_minx: float;
    mutable m_miny: float;
    mutable m_maxx: float;
    mutable m_maxy: float;
  }

  type bounds = {
    minx: float;
    miny: float;
    maxx: float;
    maxy: float;
  }

  let make_bounds () =
    { m_minx = max_float; m_miny = max_float;
      m_maxx = -.max_float; m_maxy = -.max_float}

  let point_equals ~tol p1x p1y p2x p2y =
    let dx = p1x -. p2x and dy = p1y -. p2y in
    dx *. dx +. dy *. dy < tol *. tol

  let update_bounds b x y =
    b.m_minx <- minf b.m_minx x;
    b.m_miny <- minf b.m_miny y;
    b.m_maxx <- maxf b.m_maxx x;
    b.m_maxy <- maxf b.m_maxy y

  let freeze_bounds b =
    { minx = b.m_minx; miny = b.m_miny;
      maxx = b.m_maxx; maxy = b.m_maxy }

  let freeze_path t bounds p =
    let p1 = p.path_first in
    let p0 = p1 + p.path_count - 1 in
    let p0 =
      if point_equals (T.dist_tol t)
          (T.get_x t p0) (T.get_y t p0) (T.get_x t p1) (T.get_y t p1) then
        begin
          p.path_closed <- true;
          p.path_count <- p.path_count - 1;
          p0 - 1
        end
      else p0
    in
    if p.path_count > 2 then begin
      let area = T.points_area t p in
      let reverse =
        match p.path_winding with
        | CCW -> area < 0.0
        | CW  -> area > 0.0
      in
      if reverse then
        T.reverse_points t p
    end;
    T.prepare_aux t;
    let p0 = ref p0 and p1 = ref p1 in
    for i = 0 to p.path_count - 1 do
      let x0 = T.get_x t !p0 and y0 = T.get_y t !p0 in
      update_bounds bounds x0 y0;

      let x1 = T.get_x t !p1 and y1 = T.get_y t !p1 in
      let dx = x1 -. x0 and dy = y1 -. y0 in
      let len = sqrt (dx *. dx +. dy *. dy) in
      if len > 1e-6
      then T.aux_set_d t !p0 (dx /. len) (dy /. len) len
      else T.aux_set_d t !p0 dx dy len;

      p0 := !p1;
      incr p1
    done

  let flush t =
    let bounds = make_bounds () in
    let paths = T.flush_paths t in
    List.iter (freeze_path t bounds) paths;
    freeze_bounds bounds, paths

  let last_x = T.last_x
  let last_y = T.last_y

  let set_tol = T.set_tol
  let set_tess_tol = T.set_tess_tol
  let clear = T.clear

  let has_path = T.has_path

  let close_path t =
    (T.current_path t).path_closed <- true

  let set_winding t w =
    (T.current_path t).path_winding <- w

  let move_to t x y =
    T.add_path t;
    T.add_point t x y flag_corner

  let line_to t x y =
    T.add_point t x y flag_corner

  let bezier_buf = Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout 80

  let bezier_loop t =
    let level = ref 0 in
    while !level >= 0 do
      let   x1 = bezier_buf.{!level * 8 + 0} in
      let   y1 = bezier_buf.{!level * 8 + 1} in
      let   x2 = bezier_buf.{!level * 8 + 2} in
      let   y2 = bezier_buf.{!level * 8 + 3} in
      let   x3 = bezier_buf.{!level * 8 + 4} in
      let   y3 = bezier_buf.{!level * 8 + 5} in
      let   x4 = bezier_buf.{!level * 8 + 6} in
      let   y4 = bezier_buf.{!level * 8 + 7} in
      let  x12 = ( x1 +.  x2) *. 0.5 in
      let  y12 = ( y1 +.  y2) *. 0.5 in
      let  x23 = ( x2 +.  x3) *. 0.5 in
      let  y23 = ( y2 +.  y3) *. 0.5 in
      let  x34 = ( x3 +.  x4) *. 0.5 in
      let  y34 = ( y3 +.  y4) *. 0.5 in
      let x123 = (x12 +. x23) *. 0.5 in
      let y123 = (y12 +. y23) *. 0.5 in
      let dx = x4 -. x1 in
      let dy = y4 -. y1 in
      let d2 = abs_float ((x2 -. x4) *. dy -. (y2 -. y4) *. dx) in
      let d3 = abs_float ((x3 -. x4) *. dy -. (y3 -. y4) *. dx) in
      if (d2 +. d3) *. (d2 +. d3) <= T.tess_tol t *. (dx *. dx +. dy *. dy) || !level = 8 then
        (T.add_point t x4 y4 0; decr level)
      else begin
        let  x234 = ( x23 +.  x34) *. 0.5 in
        let  y234 = ( y23 +.  y34) *. 0.5 in
        let x1234 = (x123 +. x234) *. 0.5 in
        let y1234 = (y123 +. y234) *. 0.5 in
        bezier_buf.{!level * 8 + 0} <- x1234;
        bezier_buf.{!level * 8 + 1} <- y1234;
        bezier_buf.{!level * 8 + 2} <- x234;
        bezier_buf.{!level * 8 + 3} <- y234;
        bezier_buf.{!level * 8 + 4} <- x34;
        bezier_buf.{!level * 8 + 5} <- y34;
        bezier_buf.{!level * 8 + 6} <- x4;
        bezier_buf.{!level * 8 + 7} <- y4;
        incr level;
        bezier_buf.{!level * 8 + 0} <- x1;
        bezier_buf.{!level * 8 + 1} <- y1;
        bezier_buf.{!level * 8 + 2} <- x12;
        bezier_buf.{!level * 8 + 3} <- y12;
        bezier_buf.{!level * 8 + 4} <- x123;
        bezier_buf.{!level * 8 + 5} <- y123;
        bezier_buf.{!level * 8 + 6} <- x1234;
        bezier_buf.{!level * 8 + 7} <- y1234;
      end
    done

  let bezier_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    bezier_buf.{0} <- (last_x t);
    bezier_buf.{1} <- (last_y t);
    bezier_buf.{2} <- x1;
    bezier_buf.{3} <- y1;
    bezier_buf.{4} <- x2;
    bezier_buf.{5} <- y2;
    bezier_buf.{6} <- x3;
    bezier_buf.{7} <- y3;
    bezier_loop t;
    T.set_last_flags t flag_corner

  (* Calculate which joins needs extra vertices to append, and gather vertex count. *)
  let calculate_joins t w line_join miter_limit p =
    let iw = if w > 0.0 then 1.0 /. w else 0.0 in
    (*Printf.printf "w: %f, iw: %f\n" w iw;*)
    let nleft = ref 0 in
    let nbevel = ref 0 in
    let first = p.path_first in
    let last = first + p.path_count - 1 in
    for p1 = first to last do
      let p0 = if p1 = first then last else p1 - 1 in
      let dx0 = t.T.points_aux.{5 * p0 + 0} and dy0 = t.T.points_aux.{5 * p0 + 1} in
      let dx1 = t.T.points_aux.{5 * p1 + 0} and dy1 = t.T.points_aux.{5 * p1 + 1} in

      let dmx = (dy0 +. dy1) *. 0.5 in
      let dmy = -. (dx0 +. dx1) *. 0.5 in

      let dmr2 = dmx *. dmx +. dmy *. dmy in
      let scale =
        if dmr2 <= 1e-6 then 1.0 else
          minf (1.0 /. dmr2) 600.0
      in
      t.T.points_aux.{5 * p1 + 3} <- (dmx *. scale);
      t.T.points_aux.{5 * p1 + 4} <- (dmy *. scale);

      (*Printf.printf "calculate_joins: %f %f\n" (dmx *. scale) (dmy *. scale);*)
      (*Printf.printf "dmr2: %f\n" dmr2;*)

      let flags = T.get_flags t p1 land flag_corner in
      (*Printf.printf "flags(in): %d\n" flags;*)

      let flags =
        let cross = dx1 *. dy0 -. dx0 *. dy1 in
        if cross > 0.0 then
          (incr nleft; flags lor flag_left)
        else flags
      in

      let flags =
        let dlen0 = t.points_aux.{5 * p0 + 2}
        and dlen1 = t.points_aux.{5 * p1 + 2} in
        if maxf dlen0 dlen1 > w then
          let limit = maxf 1.01 (minf dlen0 dlen1 *. iw) in
          (*Printf.printf "limit: %f, dlen0: %f, dlen1: %f\n" limit dlen0 dlen1;*)
          if dmr2 *. limit *. limit < 1.0 then
            flags lor flag_innerbevel
          else flags
        else flags
      in

      let flags =
        if (flags land flag_corner <> 0) &&
           (line_join || (dmr2 *. miter_limit *. miter_limit) < 1.0) then
          flags lor flag_bevel
        else flags
      in

      if flags land (flag_bevel lor flag_innerbevel) <> 0 then
        incr nbevel;

      (*Printf.printf "flags(out): %d\n" flags;*)

      T.set_flags t p1 flags;
    done;

    p.path_nbevel <- !nbevel;
    p.path_convex <- !nleft = p.path_count

  let calculate_joins t ~width ~line_join ~miter_limit paths =
    let line_join = match line_join with `BEVEL | `ROUND -> true | _ -> false in
    List.iter (calculate_joins t width line_join miter_limit) paths

  let get_x     = T.get_x
  let get_y     = T.get_y
  let get_flags = T.get_flags
  let get_dx    = T.aux_dx
  let get_dy    = T.aux_dy
  let get_dlen  = T.aux_dlen
  let get_dmx   = T.aux_dmx
  let get_dmy   = T.aux_dmy
  let tess_tol  = T.tess_tol
  let observed_tol = T.observed_tol

end

module B = struct
  type bigarray =
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = {
    mutable data: bigarray;
    mutable cursor: int;
  }

  let make () = {
    data = ba_empty;
    cursor = 0;
  }

  let data t = t.data
  let clear t = t.cursor <- 0

  let reserve t size =
    let total = t.cursor + size in
    if BA.dim t.data < total then
      let data' = BA.create Bigarray.float32 Bigarray.c_layout
          (total * 3 / 2)
      in
      BA.blit t.data (BA.sub data' 0 (BA.dim t.data));
      t.data <- data'

  let alloc t n =
    let x = t.cursor in
    t.cursor <- x + n;
    x

  let release t n =
    t.cursor <- t.cursor - n

  let offset t = t.cursor

  let sub t =
    BA.sub t.data 0 t.cursor
end

module V = struct

  let pi = 3.14159265358979323846264338327

  let vbuffer_put (b : B.t) ~x ~y ~dx ~dy ~u =
    let data = B.data b and c = B.alloc b 4 in
    (*Printf.printf "vbuffer_put %f %f %f %f\n" x y u v;*)
    data.{c + 0} <- x +. dx;
    data.{c + 1} <- y +. dy;
    data.{c + 2} <- float u /. 2.0;
    data.{c + 3} <- 1.0

  let dvbuffer_put ?(v=2) (b : B.t) ~x ~y ~dx ~dy ~u =
    if not (abs_float dx +. abs_float dy > 0.0) then (
      (*prerr_endline (Printexc.raw_backtrace_to_string (Printexc.get_callstack 30))*)
    );
    let data = B.data b and c = B.alloc b 4 in
    (*Printf.printf "dvbuffer_put(encode) %f+%f %f+%f %d\n" x dx y dy u;*)
    (*assert (abs_float dx < 4.0);
      assert (abs_float dy < 4.0);*)
    data.{c + 0} <- x;
    data.{c + 1} <- y;
    data.{c + 2} <- dx /. 8.0 -. 1.5 -. float u;
    data.{c + 3} <- dy /. 8.0 -. 1.5 -. float v

  let choose_bevel bevel ~w t p0 p1 =
    if bevel then
      (+. T.get_dy t p0 *. w,
       -. T.get_dx t p0 *. w,
       +. T.get_dy t p1 *. w,
       -. T.get_dx t p1 *. w)
    else
      (T.get_dmx t p1 *. w,
       T.get_dmy t p1 *. w,
       T.get_dmx t p1 *. w,
       T.get_dmy t p1 *. w)

  let dbevel_join vb t p0 p1 lw rw lu =
    let x1 = T.get_x t p1 in
    let y1 = T.get_y t p1 in
    let dlx0 = +. T.get_dy t p0 in
    let dly0 = -. T.get_dx t p0 in
    let dlx1 = +. T.get_dy t p1 in
    let dly1 = -. T.get_dx t p1 in
    let flags = T.get_flags t p1 in
    if flags land T.flag_left <> 0 then begin
      let lx0, ly0, lx1, ly1 =
        choose_bevel (flags land T.flag_innerbevel <> 0) ~w:lw t p0 p1 in

      dvbuffer_put vb ~x:x1 ~dx:lx0 ~y:y1 ~dy:ly0 ~u:lu;
      dvbuffer_put vb
        ~x:x1 ~dx:(-. dlx0 *. rw)
        ~y:y1 ~dy:(-. dly0 *. rw)
        ~u:2;

      if flags land T.flag_bevel <> 0 then begin
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:lx0 ~dy:ly0 ~u:lu;
        dvbuffer_put vb
          ~x:x1 ~dx:(-. dlx0 *. rw)
          ~y:y1 ~dy:(-. dly0 *. rw)
          ~u:2;
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:lx1 ~dy:ly1 ~u:lu;
        dvbuffer_put vb ~x:x1 ~dx:(-.dlx1 *. rw) ~y:y1 ~dy:(-.dly1 *. rw) ~u:2;
      end else begin
        let rx0 = -. T.get_dmx t p1 *. rw in
        let ry0 = -. T.get_dmy t p1 *. rw in
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;
        dvbuffer_put vb ~x:x1 ~dx:(-.dlx0 *. rw) ~y:y1 ~dy:(-.dly0 *. rw) ~u:2;
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;

        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;
        dvbuffer_put vb ~x:x1 ~dx:(-.dlx1 *. rw) ~y:y1 ~dy:(-.dly1 *. rw) ~u:2;
      end;

      dvbuffer_put vb ~x:x1 ~y:y1 ~dx:lx1 ~dy:ly1 ~u:lu;
      dvbuffer_put vb ~x:x1 ~dx:(-.dlx1 *. rw) ~y:y1 ~dy:(-.dly1 *. rw) ~u:2
    end else begin
      let rx0, ry0, rx1, ry1 =
        choose_bevel (flags land T.flag_innerbevel <> 0) ~w:(-.rw) t p0 p1 in

      dvbuffer_put vb ~x:x1 ~dx:(dlx0 *. lw) ~y:y1 ~dy:(dly0 *. lw) ~u:lu;
      dvbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;
      dvbuffer_put vb ~x:x1 ~dx:(dlx0 *. lw) ~y:y1 ~dy:(dly0 *. lw) ~u:lu;

      if flags land T.flag_bevel <> 0 then begin
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;

        dvbuffer_put vb ~x:x1 ~dx:(dlx1 *. lw) ~y:y1 ~dy:(dly1 *. lw) ~u:lu;
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:rx1 ~dy:ry1 ~u:2;
      end else begin
        let lx0 = T.get_dmx t p1 *. lw in
        let ly0 = T.get_dmy t p1 *. lw in
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;

        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:lx0 ~dy:ly0 ~u:lu;
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:lx0 ~dy:ly0 ~u:lu;

        dvbuffer_put vb ~x:x1 ~dx:(dlx1 *. lw) ~y:y1 ~dy:(dly1 *. lw) ~u:lu;
        dvbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;
      end;

      dvbuffer_put vb ~x:x1 ~dx:(dlx1 *. lw) ~y:y1 ~dy:(dly1 *. lw) ~u:lu;
      dvbuffer_put vb ~x:x1 ~y:y1 ~dx:rx1 ~dy:ry1 ~u:2;
    end

  let bevel_join vb t p0 p1 lw rw lu =
    let x1 = T.get_x t p1 in
    let y1 = T.get_y t p1 in
    let dlx0 = +. T.get_dy t p0 in
    let dly0 = -. T.get_dx t p0 in
    let dlx1 = +. T.get_dy t p1 in
    let dly1 = -. T.get_dx t p1 in
    let flags = T.get_flags t p1 in
    if flags land T.flag_left <> 0 then begin
      let lx0, ly0, lx1, ly1 =
        choose_bevel (flags land T.flag_innerbevel <> 0) ~w:lw t p0 p1 in

      vbuffer_put vb ~x:x1 ~dx:lx0 ~y:y1 ~dy:ly0 ~u:lu;
      vbuffer_put vb
        ~x:x1 ~dx:(-. dlx0 *. rw)
        ~y:y1 ~dy:(-. dly0 *. rw)
        ~u:2;

      if flags land T.flag_bevel <> 0 then begin
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:lx0 ~dy:ly0 ~u:lu;
        vbuffer_put vb
          ~x:x1 ~dx:(-. dlx0 *. rw)
          ~y:y1 ~dy:(-. dly0 *. rw)
          ~u:2;
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:lx1 ~dy:ly1 ~u:lu;
        vbuffer_put vb ~x:x1 ~dx:(-.dlx1 *. rw) ~y:y1 ~dy:(-.dly1 *. rw) ~u:2;
      end else begin
        let rx0 = -. T.get_dmx t p1 *. rw in
        let ry0 = -. T.get_dmy t p1 *. rw in
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;
        vbuffer_put vb ~x:x1 ~dx:(-.dlx0 *. rw) ~y:y1 ~dy:(-.dly0 *. rw) ~u:2;
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;

        vbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;
        vbuffer_put vb ~x:x1 ~dx:(-.dlx1 *. rw) ~y:y1 ~dy:(-.dly1 *. rw) ~u:2;
      end;

      vbuffer_put vb ~x:x1 ~y:y1 ~dx:lx1 ~dy:ly1 ~u:lu;
      vbuffer_put vb ~x:x1 ~dx:(-.dlx1 *. rw) ~y:y1 ~dy:(-.dly1 *. rw) ~u:2
    end else begin
      let rx0, ry0, rx1, ry1 =
        choose_bevel (flags land T.flag_innerbevel <> 0) ~w:(-.rw) t p0 p1 in

      vbuffer_put vb ~x:x1 ~dx:(dlx0 *. lw) ~y:y1 ~dy:(dly0 *. lw) ~u:lu;
      vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;
      vbuffer_put vb ~x:x1 ~dx:(dlx0 *. lw) ~y:y1 ~dy:(dly0 *. lw) ~u:lu;

      if flags land T.flag_bevel <> 0 then begin
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;

        vbuffer_put vb ~x:x1 ~dx:(dlx1 *. lw) ~y:y1 ~dy:(dly1 *. lw) ~u:lu;
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx1 ~dy:ry1 ~u:2;
      end else begin
        let lx0 = T.get_dmx t p1 *. lw in
        let ly0 = T.get_dmy t p1 *. lw in
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;

        vbuffer_put vb ~x:x1 ~y:y1 ~dx:lx0 ~dy:ly0 ~u:lu;
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:lx0 ~dy:ly0 ~u:lu;

        vbuffer_put vb ~x:x1 ~dx:(dlx1 *. lw) ~y:y1 ~dy:(dly1 *. lw) ~u:lu;
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;
      end;

      vbuffer_put vb ~x:x1 ~dx:(dlx1 *. lw) ~y:y1 ~dy:(dly1 *. lw) ~u:lu;
      vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx1 ~dy:ry1 ~u:2;
    end

  let round_join vb t p0 p1 lw rw ncap =
    let x1 = T.get_x t p1 in
    let y1 = T.get_y t p1 in
    let dlx0 = +. T.get_dy t p0 in
    let dly0 = -. T.get_dx t p0 in
    let dlx1 = +. T.get_dy t p1 in
    let dly1 = -. T.get_dx t p1 in
    let flags = T.get_flags t p1 in
    if flags land T.flag_left <> 0 then begin
      let lx0, ly0, lx1, ly1 =
        choose_bevel (flags land T.flag_innerbevel <> 0) ~w:lw t p0 p1 in

      vbuffer_put vb ~x:x1 ~dx:lx0 ~y:y1 ~dy:ly0 ~u:0;
      vbuffer_put vb ~x:x1 ~dx:(-. dlx0 *. rw) ~y:y1 ~dy:(-. dly0 *. rw) ~u:2;

      let a0 = atan2 (-.dly0) (-.dlx0) in
      let a1 = atan2 (-.dly1) (-.dlx1) in
      let a1 = if a1 > a0 then a1 -. pi *. 2.0 else a1 in
      let n = int_of_float (ceil (((a0 -. a1) /. pi) *. float ncap)) in
      let n = if n <= 2 then 2 else if n >= ncap then ncap else n in
      for i = 0 to n - 1 do
        let u = float i /. float (n - 1) in
        let a = a0 +. u *. (a1 -. a0) in
        let rx = cos a *. rw in
        let ry = sin a *. rw in
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx ~dy:ry ~u:2;
      done;

      vbuffer_put vb ~x:x1 ~y:y1 ~dx:lx1 ~dy:ly1 ~u:0;
      vbuffer_put vb ~x:x1 ~dx:(-. dlx1 *. rw) ~y:y1 ~dy:(-. dly1 *. rw) ~u:2;

    end else begin
      let rx0, ry0, rx1, ry1 =
        choose_bevel (flags land T.flag_innerbevel <> 0) ~w:(-.rw) t p0 p1 in

      vbuffer_put vb ~x:x1 ~dx:(dlx0 *. rw) ~y:y1 ~dy:(dly0 *. rw) ~u:0;
      vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx0 ~dy:ry0 ~u:2;

      let a0 = atan2 dly0 dlx0 in
      let a1 = atan2 dly1 dlx1 in
      let a1 = if a1 < a0 then a1 +. pi *. 2.0 else a1 in
      let n = int_of_float (ceil (((a1 -. a0) /. pi) *. float ncap)) in
      let n = if n <= 2 then 2 else if n >= ncap then ncap else n in
      for i = 0 to n - 1 do
        let u = float i /. float (n - 1) in
        let a = a0 +. u *. (a1 -. a0) in
        let lx = cos a *. lw in
        let ly = sin a *. lw in
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:lx ~dy:ly ~u:0;
        vbuffer_put vb ~x:x1 ~y:y1 ~dx:0.0 ~dy:0.0 ~u:1;
      done;

      vbuffer_put vb ~x:x1 ~dx:(dlx1 *. rw) ~y:y1 ~dy:(dly1 *. rw) ~u:0;
      vbuffer_put vb ~x:x1 ~y:y1 ~dx:rx1 ~dy:ry1 ~u:2;
    end

  (* Expand fill *)

  type path = {
    convex: bool;
    fill_first: int;
    fill_count: int;
    stroke_first: int;
    stroke_count: int;
  }

  let rec sum f acc = function
    | [] -> acc
    | x :: xs -> sum f (acc + f x) xs

  let sum f xs = sum f 0 xs

  module Fill = struct

    let count_no_fringe {T. path_count; path_nbevel} =
      (path_count + path_nbevel + 1)

    let count_fringe {T. path_count; path_nbevel} =
      (path_count + path_nbevel + 1) + (path_count + path_nbevel * 5 + 1) * 2

    let no_fringe t vb path =
      let fill_first = B.offset vb / 4 in
      for i = path.T.path_first to path.T.path_first + path.T.path_count - 1 do
        vbuffer_put vb ~x:(T.get_x t i) ~y:(T.get_y t i) ~dx:0.0 ~dy:0.0 ~u:1
      done;
      let fill_count = (B.offset vb / 4 - fill_first) in
      { convex = path.T.path_convex;
        fill_first; fill_count;
        stroke_first = 0; stroke_count = 0;
      }

    let fringe t vb ~convex path =
      let fill_first = B.offset vb / 4 in
      let first = path.T.path_first in
      let last = first + path.T.path_count - 1 in
      for p1 = first to last do
        let p0 = if p1 = first then last else p1 - 1 in
        let flags = T.get_flags t p1 in
        let x1 = T.get_x t p1 in
        let y1 = T.get_y t p1 in
        if flags land T.flag_bevel <> 0 then begin
          if flags land T.flag_left <> 0 then
            dvbuffer_put vb
              ~x:x1 ~dx:(T.get_dmx t p1 *. 0.5)
              ~y:y1 ~dy:(T.get_dmy t p1 *. 0.5)
              ~u:1
          else begin
            dvbuffer_put vb
              ~x:x1 ~dx:(+. T.get_dy t p0 *. 0.5)
              ~y:y1 ~dy:(-. T.get_dx t p0 *. 0.5)
              ~u:1;
            dvbuffer_put vb
              ~x:x1 ~dx:(+. T.get_dy t p1 *. 0.5)
              ~y:y1 ~dy:(-. T.get_dx t p1 *. 0.5)
              ~u:1
          end
        end else (
          dvbuffer_put vb
            ~x:x1
            ~y:y1
            ~dx:(T.get_dmx t p1 *. 0.5)
            ~dy:(T.get_dmy t p1 *. 0.5)
            ~u:1
        )
      done;
      let stroke_first = B.offset vb / 4 in
      (* Calculate fringe *)
      let lw, lu = if convex then 0.5, 1 else 1.5, 0 in
      for p1 = first to last do
        let p0 = if p1 = first then last else p1 - 1 in
        if T.get_flags t p1 land (T.flag_bevel lor T.flag_innerbevel) <> 0
        then
          dbevel_join vb t p0 p1 lw 0.5 lu
        else begin
          let x1 = T.get_x t p1 in
          let y1 = T.get_y t p1 in
          (*Printf.printf "fringe_stroke: %f %f %f %f\n"
            (x1 +. T.get_dmx t p1 *. lw)
            (y1 +. T.get_dmy t p1 *. lw)
            lu 1.0;*)
          dvbuffer_put vb
            ~x:x1 ~dx:(T.get_dmx t p1 *. lw)
            ~y:y1 ~dy:(T.get_dmy t p1 *. lw)
            ~u:lu;
          (*Printf.printf "fringe_stroke: %f %f %f %f\n"
            (x1 -. T.get_dmx t p1 *. rw)
            (y1 -. T.get_dmy t p1 *. rw)
            ru 1.0;*)
          dvbuffer_put vb
            ~x:x1 ~dx:(-. T.get_dmx t p1 *. 0.5)
            ~y:y1 ~dy:(-. T.get_dmy t p1 *. 0.5)
            ~u:2
        end
      done;
      let data = B.data vb and c = B.alloc vb 8 in
      (* Loop it *)
      data.{c + 0} <- data.{stroke_first * 4 + 0};
      data.{c + 1} <- data.{stroke_first * 4 + 1};
      data.{c + 2} <- data.{stroke_first * 4 + 2};
      data.{c + 3} <- data.{stroke_first * 4 + 3};
      data.{c + 4} <- data.{stroke_first * 4 + 4};
      data.{c + 5} <- data.{stroke_first * 4 + 5};
      data.{c + 6} <- data.{stroke_first * 4 + 6};
      data.{c + 7} <- data.{stroke_first * 4 + 7};
      let stroke_last = B.offset vb / 4 in
      { convex = path.T.path_convex;
        fill_first; stroke_first;
        fill_count   = stroke_first - fill_first;
        stroke_count = stroke_last  - stroke_first;
      }

    let expand_no_aa t vb paths =
      B.reserve vb (sum count_no_fringe paths * 4);
      List.map (no_fringe t vb) paths

    let expand_aa t vb paths =
      B.reserve vb (sum count_fringe paths * 4);
      let convex = match paths with
        | [p] -> p.T.path_convex
        |  _  -> false
      in
      List.map (fringe t vb ~convex) paths
  end

  let fill t vb ~edge_antialias ~scale paths =
    T.calculate_joins t ~width:scale ~line_join:`MITER ~miter_limit:2.4 paths;
    if edge_antialias then
      Fill.expand_aa t vb paths
    else
      Fill.expand_no_aa t vb paths

  module Stroke = struct

    let curve_divs r arc tol =
      let da = acos (r /. (r +. tol)) *. 2.0 in
      int_of_float (maxf 2.0 (ceil (arc /. da)))

    let count join_ncap cap_ncap {T. path_count; path_nbevel; path_closed} =
      (*Printf.printf "path_count: %d, path_nbevel: %d\n" path_count path_nbevel;*)
      ((path_count + path_nbevel * (join_ncap + 2) + 1) * 2) +
      (if path_closed then 0 else (cap_ncap * 2 + 2) * 2)

    let count ~line_join ~line_cap ncap =
      count
        (match line_join with
         | `ROUND -> ncap
         | _ -> 3)
        (match line_cap with
         | `ROUND -> ncap
         | _ -> 2)

    let roundcap_end vb t p ~dx ~dy ~width ~ncap =
      (*Printf.printf "roundcap_end %f %f %f %d\n" dx dy w ncap;*)
      let px = T.get_x t p and py = T.get_y t p in
      let dlx = dy and dly = -.dx in
      dvbuffer_put vb ~x:(px +. dlx *. width) ~y:(py +. dly *. width) ~dx:(dlx *. 0.5) ~dy:(dly *. 0.5) ~u:0;
      dvbuffer_put vb ~x:(px -. dlx *. width) ~y:(py -. dly *. width) ~dx:(-. dlx *. 0.5) ~dy:(-. dly *. 0.5) ~u:2;
      for i = 0 to ncap - 1 do
        let a = float i /. float (ncap - 1) *. pi in
        let ax = cos a and ay = sin a in
        let fx = dx *. ay -. dlx *. ax in
        let fy = dy *. ay -. dly *. ax in
        dvbuffer_put vb ~x:px ~y:py ~dx:0.0 ~dy:0.0 ~u:1;
        dvbuffer_put vb
          ~x:(px +. fx *. width) ~dx:(fx *. 0.5)
          ~y:(py +. fy *. width) ~dy:(fy *. 0.5)
          ~u:2;
      done

    let roundcap_start vb t p ~dx ~dy ~width ~ncap =
      (*Printf.printf "roundcap_start %f %f %f %d\n" dx dy w ncap;*)
      let px = T.get_x t p and py = T.get_y t p in
      let dlx = dy and dly = -.dx in
      for i = 0 to ncap - 1 do
        let a = float i /. float (ncap - 1) *. pi in
        let ax = cos a and ay = sin a in
        let fx = dlx *. ax +. dx *. ay in
        let fy = dly *. ax +. dy *. ay in
        dvbuffer_put vb
          ~x:(px -. fx *. width) ~dx:(-. fx *. 0.5)
          ~y:(py -. fy *. width) ~dy:(-. fy *. 0.5)
          ~u:0;
        dvbuffer_put vb ~x:px ~y:py ~dx:0.0 ~dy:0.0 ~u:1;
      done;
      dvbuffer_put vb ~x:(px +. dlx *. width) ~y:(py +. dly *. width) ~dx:(dlx *. 0.5) ~dy:(dly *. 0.5) ~u:0;
      dvbuffer_put vb ~x:(px -. dlx *. width) ~y:(py -. dly *. width) ~dx:(-. dlx *. 0.5) ~dy:(-. dly *. 0.5)  ~u:2

    let buttcap_start vb t p ~dx ~dy ~d ~dd ~width =
      (*Printf.printf "buttcap_start %f %f %f %f %f\n" dx dy w d aa;*)
      let px = T.get_x t p -. dx *. d in
      let py = T.get_y t p -. dy *. d in
      let dlx = dy *. width and dly = -.dx *. width in
      dvbuffer_put vb ~u:0 ~v:0
        ~x:(px +. dlx) ~dx:(-. dx -. dx *. dd)
        ~y:(py +. dly) ~dy:(-. dy -. dy *. dd);
      dvbuffer_put vb ~u:2 ~v:0
        ~x:(px -. dlx) ~dx:(-. dx -. dx *. dd)
        ~y:(py -. dly) ~dy:(-. dy -. dy *. dd);
      dvbuffer_put vb ~u:0
        ~x:(px +. dlx) ~dx:(-. dx *. dd)
        ~y:(py +. dly) ~dy:(-. dy *. dd);
      dvbuffer_put vb ~u:2
        ~x:(px -. dlx) ~dx:(-. dx *. dd)
        ~y:(py -. dly) ~dy:(-. dy *. dd)

    let buttcap_end vb t p ~dx ~dy ~d ~dd ~width =
      (*Printf.printf "buttcap_end %f %f %f %f %f\n" dx dy w d aa;*)
      let px = T.get_x t p +. dx *. d in
      let py = T.get_y t p +. dy *. d in
      let dlx = dy *. width and dly = -.dx *. width in
      dvbuffer_put vb ~u:0 ~x:(px +. dlx) ~y:(py +. dly) ~dx:(dx *. dd) ~dy:(dy *. dd);
      dvbuffer_put vb ~u:2 ~x:(px -. dlx) ~y:(py -. dly) ~dx:(dx *. dd) ~dy:(dy *. dd);
      dvbuffer_put vb ~u:0 ~x:(px +. dlx) ~dx:(dx *. dd +. dx) ~y:(py +. dly) ~dy:(dy *. dd +. dy) ~v:0;
      dvbuffer_put vb ~u:2 ~x:(px -. dlx) ~dx:(dx *. dd +. dx) ~y:(py -. dly) ~dy:(dy *. dd +. dy) ~v:0

    let expand_path t vb ~line_join ~line_cap ~width ncap path =
      let stroke_first = B.offset vb / 4 in
      let first = path.T.path_first in
      let last = first + path.T.path_count - 1 in
      if not path.T.path_closed then begin
        let p0 = first and p1 = first + 1 in
        let dx = T.get_x t p1 -. T.get_x t p0 in
        let dy = T.get_y t p1 -. T.get_y t p0 in
        let dlen = sqrt (dx *. dx +. dy *. dy) in
        let dlen = if dlen < 1e-6 then 1. else 1. /. dlen in
        let dx = dx *. dlen and dy = dy *. dlen in
        match line_cap with
        | `BUTT   -> buttcap_start  vb t p0 ~dx ~dy ~width ~d:0.0 ~dd:(-.0.5)
        | `SQUARE -> buttcap_start  vb t p0 ~dx ~dy ~width ~d:width ~dd:(-. 1.0)
        | `ROUND  -> roundcap_start vb t p0 ~dx ~dy ~width ~ncap
      end;
      let s, e =
        if path.T.path_closed then
          first, last
        else
          first + 1, last - 1
      in
      for p1 = s to e do
        if T.get_flags t p1 land (T.flag_bevel lor T.flag_innerbevel) <> 0 then begin
          let p0 = if p1 = first then last else p1 - 1 in
          if line_join = `ROUND then
            round_join vb t p0 p1 width width ncap
          else
            bevel_join vb t p0 p1 width width 0
        end else begin
          let x1 = T.get_x t p1 and dmx = T.get_dmx t p1 in
          let y1 = T.get_y t p1 and dmy = T.get_dmy t p1 in
          let dx = dmx *. width and dy = dmy *. width in
          dvbuffer_put vb ~x:(x1 +. dx) ~y:(y1 +. dy) ~dx:(+. dmx *. 0.5) ~dy:(+. dmy *. 0.5) ~u:0;
          dvbuffer_put vb ~x:(x1 -. dx) ~y:(y1 -. dy) ~dx:(-. dmx *. 0.5) ~dy:(-. dmy *. 0.5) ~u:2;
        end
      done;

      if path.T.path_closed then begin
        (* Loop it *)
        let data = B.data vb and c = B.alloc vb 8 in
        data.{c + 0} <- data.{stroke_first * 4 + 0};
        data.{c + 1} <- data.{stroke_first * 4 + 1};
        data.{c + 2} <- data.{stroke_first * 4 + 2};
        data.{c + 3} <- data.{stroke_first * 4 + 3};
        data.{c + 4} <- data.{stroke_first * 4 + 4};
        data.{c + 5} <- data.{stroke_first * 4 + 5};
        data.{c + 6} <- data.{stroke_first * 4 + 6};
        data.{c + 7} <- data.{stroke_first * 4 + 7};
      end else begin
        let p0 = last - 1 and p1 = last in
        let dx = T.get_x t p1 -. T.get_x t p0 in
        let dy = T.get_y t p1 -. T.get_y t p0 in
        let dlen = sqrt (dx *. dx +. dy *. dy) in
        let dlen = if dlen < 1e-6 then 1. else 1. /. dlen in
        let dx = dx *. dlen and dy = dy *. dlen in
        match line_cap with
        | `BUTT   -> buttcap_end  vb t p1 ~dx ~dy ~width ~d:0.0 ~dd:(-0.5)
        | `SQUARE -> buttcap_end  vb t p1 ~dx ~dy ~width ~d:width ~dd:(-1.0)
        | `ROUND  -> roundcap_end vb t p1 ~dx ~dy ~width ~ncap
      end;
      { convex = false;
        fill_first = 0; fill_count = 0;
        stroke_first; stroke_count = B.offset vb / 4 - stroke_first;
      }

    let expand t vb ~line_join ~line_cap ~width ~miter_limit paths =
      let ncap = curve_divs width pi (T.tess_tol t) in
      T.calculate_joins t ~width ~line_join ~miter_limit paths;
      let count = sum (count ~line_join ~line_cap ncap) paths * 4 in
      (*Printf.printf "count:%d w:%f\n" count w;*)
      B.reserve vb count;
      List.map (expand_path t vb ~line_join ~line_cap ~width ncap) paths
  end

  let stroke t vb ~width ~line_join ~line_cap ~miter_limit paths =
    let width = width *. 0.5 in
    Stroke.expand t vb ~line_join ~line_cap ~miter_limit ~width paths
end
