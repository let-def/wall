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

module BA = Bigarray.Array1
let ba_empty = BA.create Bigarray.float32 Bigarray.c_layout 0

(* Algorithms from
   https://github.com/memononen/nanovg/blob/master/src/nanovg.c *)

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
  let flag_bevel       = 0x2
  let flag_left        = 0x4
  let flag_innerbevel  = 0x8

  module T : sig
    type t

    val make : unit -> t
    val clear : t -> unit

    val set_tol : t -> dist:float -> tess:float -> unit
    val dist_tol : t -> float
    val tess_tol : t -> float

    val add_point : t -> float -> float -> int -> unit

    val reverse_points : t -> path -> unit
    val points_area : t -> path -> float

    val add_path : t -> unit
    val flush_paths : t -> path list

    val has_path : t -> bool
    val current_path : t -> path

    val last_x : t -> float
    val last_y : t -> float

    val get_x : t -> int -> float
    val get_y : t -> int -> float

    val get_flags : t -> int -> int
    val set_flags : t -> int -> int -> unit

    val prepare_aux : t -> unit

    val aux_set_d : t -> int -> float -> float -> float -> unit
    val aux_set_dm : t -> int -> float -> float -> unit

    val aux_dx : t -> int -> float
    val aux_dy : t -> int -> float
    val aux_dlen : t -> int -> float
    val aux_dmx : t -> int -> float
    val aux_dmy : t -> int -> float
  end = struct

    type fbuffer =
      (float, Bigarray.float32_elt, Bigarray.c_layout) BA.t

    type u8buffer =
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) BA.t

    type t = {
      mutable dist_tol : float;
      mutable tess_tol : float;
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
      points_aux = ba_empty
    }

    let clear t =
      t.point <- 0;
      t.paths <- []

    let set_tol t ~dist ~tess =
      t.dist_tol <- dist;
      t.tess_tol <- tess

    let dist_tol t = t.dist_tol
    let tess_tol t = t.tess_tol

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
      | [] -> invalid_arg "Wall_geom.current_path"
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

    let aux_set_dm {points_aux} pt dmx dmy =
      points_aux.{5 * pt + 3} <- dmx;
      points_aux.{5 * pt + 4} <- dmy

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
    b.m_minx <- min b.m_minx x;
    b.m_miny <- min b.m_miny y;
    b.m_maxx <- max b.m_maxx x;
    b.m_maxy <- max b.m_maxy y

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

  let rec bezier_to t x1 y1 x2 y2 x3 y3 x4 y4 level flags =
    if level <= 10 then begin
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

      if (d2 +. d3) *. (d2 +. d3) < T.tess_tol t *. (dx *. dx +. dy *. dy) then
        T.add_point t x4 y4 flags
      else begin
        let  x234 = ( x23 +.  x34) *. 0.5 in
        let  y234 = ( y23 +.  y34) *. 0.5 in
        let x1234 = (x123 +. x234) *. 0.5 in
        let y1234 = (y123 +. y234) *. 0.5 in
        let level = level + 1 in
        bezier_to t    x1    y1  x12  y12 x123 y123 x1234 y1234 level 0;
        bezier_to t x1234 y1234 x234 y234  x34  y34    x4    y4 level flags
      end
    end

  let bezier_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    bezier_to t (last_x t) (last_y t) x1 y1 x2 y2 x3 y3 0 flag_corner

  (* Calculate which joins needs extra vertices to append, and gather vertex count. *)
  let calculate_joins t w line_join miter_limit p =
    let iw = if w > 0.0 then 1.0 /. w else 0.0 in
    let nleft = ref 0 in
    let nbevel = ref 0 in
    let first = p.path_first in
    let last = first + p.path_count - 1 in
    for p1 = first to last do
      let p0 = if p1 = first then last else p1 - 1 in
      let dx0 = T.aux_dx t p0 and dy0 = T.aux_dy t p0 in
      let dx1 = T.aux_dx t p1 and dy1 = T.aux_dy t p1 in

      let dmx = (dy0 +. dy1) *. 0.5 in
      let dmy = -. (dx0 +. dx1) *. 0.5 in

      let dmr2 = dmx *. dmx +. dmy *. dmy in
      let scale =
        if dmr2 <= 1e-6 then 1.0 else
          min (1.0 /. dmr2) 600.0
      in
      T.aux_set_dm t p1 (dmx *. scale) (dmy *. scale);

      (*Printf.printf "calculate_joins: %f %f\n" (dmx *. scale) (dmy *. scale);*)

      let flags = T.get_flags t p1 land flag_corner in

      let flags =
        let cross = dx1 *. dy0 -. dx0 *. dy1 in
        if cross > 0.0 then
          (incr nleft; flags lor flag_left)
        else flags
      in

      let flags =
        let dlen0 = T.aux_dlen t p0 and dlen1 = T.aux_dlen t p1 in
        if max dlen0 dlen1 > w then
          let limit = max 1.01 (min dlen0 dlen1 *. iw) in
          if dmr2 *. limit *. limit < 1.0 then
            flags lor flag_innerbevel
          else flags
        else flags
      in

      let flags =
        if (flags land flag_corner <> 0) &&
           ((line_join = `BEVEL || line_join = `ROUND) ||
            (dmr2 *. miter_limit *. miter_limit) < 1.0) then
          flags lor flag_bevel
        else flags
      in

      if flags land (flag_bevel lor flag_innerbevel) <> 0 then
        incr nbevel;

      T.set_flags t p1 flags;
    done;

    p.path_nbevel <- !nbevel;
    p.path_convex <- !nleft = p.path_count

  let calculate_joins t ~w ~line_join ~miter_limit paths =
    List.iter (calculate_joins t w line_join miter_limit) paths

  let get_x     = T.get_x
  let get_y     = T.get_y
  let get_flags = T.get_flags
  let get_dx    = T.aux_dx
  let get_dy    = T.aux_dy
  let get_dlen  = T.aux_dlen
  let get_dmx   = T.aux_dmx
  let get_dmy   = T.aux_dmy
  let tess_tol = T.tess_tol
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

  let prepare t size =
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

  let offset t = t.cursor

  let sub t =
    BA.sub t.data 0 t.cursor
end

module V = struct

  let pi = 3.14159265358979323846264338327

  let vbuffer_put (b : B.t) ~x ~y ~u ~v =
    let data = B.data b and c = B.alloc b 4 in
    (*Printf.printf "vbuffer_put %f %f %f %f\n" x y u v;*)
    data.{c + 0} <- x;
    data.{c + 1} <- y;
    data.{c + 2} <- u;
    data.{c + 3} <- v

  let choose_bevel bevel ~w t p0 p1 =
    let x1 = T.get_x t p1 and y1 = T.get_y t p1 in
    if bevel then
      (x1 +. T.get_dy t p0 *. w,
       y1 -. T.get_dx t p0 *. w,
       x1 +. T.get_dy t p1 *. w,
       y1 -. T.get_dx t p1 *. w)
    else
      (x1 +. T.get_dmx t p1 *. w,
       y1 +. T.get_dmy t p1 *. w,
       x1 +. T.get_dmx t p1 *. w,
       y1 +. T.get_dmy t p1 *. w)

  let bevel_join vb t p0 p1 lw rw lu ru =
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

      vbuffer_put vb ~x:lx0 ~y:ly0 ~u:lu ~v:1.0;
      vbuffer_put vb
        ~x:(x1 -. dlx0 *. rw)
        ~y:(y1 -. dly0 *. rw)
        ~u:ru ~v:1.0;

      if flags land T.flag_bevel <> 0 then begin
        vbuffer_put vb ~x:lx0 ~y:ly0 ~u:lu ~v:1.0;
        vbuffer_put vb
          ~x:(x1 -. dlx0 *. rw)
          ~y:(y1 -. dly0 *. rw)
          ~u:ru ~v:1.0;
        vbuffer_put vb ~x:lx1 ~y:ly1 ~u:lu ~v:1.0;
        vbuffer_put vb
          ~x:(x1 -. dlx1 *. rw)
          ~y:(y1 -. dly1 *. rw)
          ~u:ru ~v:1.0;
      end else begin
        let rx0 = x1 -. T.get_dmx t p1 *. rw in
        let ry0 = y1 -. T.get_dmy t p1 *. rw in
        vbuffer_put vb ~x:x1 ~y:y1 ~u:0.5 ~v:1.0;
        vbuffer_put vb
          ~x:(x1 -. dlx0 *. rw)
          ~y:(y1 -. dly0 *. rw)
          ~u:ru ~v:1.0;
        vbuffer_put vb ~x:rx0 ~y:ry0 ~u:ru ~v:1.0;
        vbuffer_put vb ~x:rx0 ~y:ry0 ~u:ru ~v:1.0;

        vbuffer_put vb ~x:x1 ~y:y1 ~u:0.5 ~v:1.0;
        vbuffer_put vb
          ~x:(x1 -. dlx1 *. rw)
          ~y:(y1 -. dly1 *. rw)
          ~u:ru ~v:1.0;
      end;

      vbuffer_put vb ~x:lx1 ~y:ly1 ~u:lu ~v:1.0;
      vbuffer_put vb
        ~x:(x1 -. dlx1 *. rw)
        ~y:(y1 -. dly1 *. rw)
        ~u:ru ~v:1.0
    end else begin
      let rx0, ry0, rx1, ry1 =
        choose_bevel (flags land T.flag_innerbevel <> 0) ~w:(-.rw) t p0 p1 in

      vbuffer_put vb
        ~x:(x1 +. dlx0 *. lw)
        ~y:(y1 +. dly0 *. lw)
        ~u:lu ~v:1.0;
      vbuffer_put vb ~x:rx0 ~y:ry0 ~u:ru ~v:1.0;

      if flags land T.flag_bevel <> 0 then begin
        vbuffer_put vb
          ~x:(x1 +. dlx0 *. lw)
          ~y:(y1 +. dly0 *. lw)
          ~u:lu ~v:1.0;
        vbuffer_put vb ~x:rx0 ~y:ry0 ~u:ru ~v:1.0;

        vbuffer_put vb
          ~x:(x1 +. dlx1 *. lw)
          ~y:(y1 +. dly1 *. lw)
          ~u:lu ~v:1.0;
        vbuffer_put vb ~x:rx1 ~y:ry1 ~u:ru ~v:1.0;
      end else begin
        let lx0 = x1 +. T.get_dmx t p1 *. lw in
        let ly0 = y1 +. T.get_dmy t p1 *. lw in
        vbuffer_put vb
          ~x:(x1 +. dlx0 *. lw)
          ~y:(y1 +. dly0 *. lw)
          ~u:lu ~v:1.0;
        vbuffer_put vb ~x:x1 ~y:y1 ~u:0.5 ~v:1.0;

        vbuffer_put vb ~x:lx0 ~y:ly0 ~u:lu ~v:1.0;
        vbuffer_put vb ~x:lx0 ~y:ly0 ~u:lu ~v:1.0;

        vbuffer_put vb
          ~x:(x1 +. dlx1 *. lw)
          ~y:(y1 +. dly1 *. lw)
          ~u:lu ~v:1.0;
        vbuffer_put vb ~x:x1 ~y:y1 ~u:0.5 ~v:1.0;
      end;

      vbuffer_put vb
        ~x:(x1 +. dlx1 *. lw)
        ~y:(y1 +. dly1 *. lw)
        ~u:lu ~v:1.0;
      vbuffer_put vb ~x:rx1 ~y:ry1 ~u:ru ~v:1.0;
    end

  let round_join vb t p0 p1 lw rw lu ru ncap =
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

      vbuffer_put vb ~x:lx0 ~y:ly0 ~u:lu ~v:1.0;
      vbuffer_put vb
        ~x:(x1 -. dlx0 *. rw)
        ~y:(y1 -. dly0 *. rw)
        ~u:ru ~v:1.0;

      let a0 = atan2 (-.dly0) (-.dlx0) in
      let a1 = atan2 (-.dly1) (-.dlx1) in
      let a1 = if a1 > a0 then a1 -. pi *. 2.0 else a1 in
      let n = int_of_float (ceil (((a0 -. a1) /. pi) *. float ncap)) in
      let n = if n <= 2 then 2 else if n >= ncap then ncap else n in
      for i = 0 to n - 1 do
        let u = float i /. float (n - 1) in
        let a = a0 +. u *. (a1 -. a0) in
        let rx = x1 +. cos a *. rw in
        let ry = y1 +. sin a *. rw in
        vbuffer_put vb ~x:x1 ~y:y1 ~u:0.5 ~v:1.0;
        vbuffer_put vb ~x:rx ~y:ry ~u:ru ~v:1.0;
      done;

      vbuffer_put vb ~x:lx1 ~y:ly1 ~u:lu ~v:1.0;
      vbuffer_put vb
        ~x:(x1 -. dlx1 *. rw)
        ~y:(y1 -. dly1 *. rw)
        ~u:ru ~v:1.0

    end else begin
      let rx0, ry0, rx1, ry1 =
        choose_bevel (flags land T.flag_innerbevel <> 0) ~w:(-.rw) t p0 p1 in

      vbuffer_put vb
        ~x:(x1 +. dlx0 *. rw)
        ~y:(y1 +. dly0 *. rw)
        ~u:lu ~v:1.0;
      vbuffer_put vb ~x:rx0 ~y:ry0 ~u:ru ~v:1.0;

      let a0 = atan2 dly0 dlx0 in
      let a1 = atan2 dly1 dlx1 in
      let a1 = if a1 < a0 then a1 +. pi *. 2.0 else a1 in
      let n = int_of_float (ceil (((a1 -. a0) /. pi) *. float ncap)) in
      let n = if n <= 2 then 2 else if n >= ncap then ncap else n in
      for i = 0 to n - 1 do
        let u = float i /. float (n - 1) in
        let a = a0 +. u *. (a1 -. a0) in
        let lx = x1 +. cos a *. lw in
        let ly = y1 +. sin a *. lw in
        vbuffer_put vb ~x:lx ~y:ly ~u:lu ~v:1.0;
        vbuffer_put vb ~x:x1 ~y:y1 ~u:0.5 ~v:1.0;
      done;

      vbuffer_put vb
        ~x:(x1 +. dlx1 *. rw)
        ~y:(y1 +. dly1 *. rw)
        ~u:lu ~v:1.0;
      vbuffer_put vb ~x:rx1 ~y:ry1 ~u:ru ~v:1.0;
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
        vbuffer_put vb ~x:(T.get_x t i) ~y:(T.get_y t i) ~u:0.5 ~v:1.0
      done;
      let fill_count = (B.offset vb / 4 - fill_first) in
      { convex = path.T.path_convex;
        fill_first; fill_count;
        stroke_first = 0; stroke_count = 0;
      }

    let fringe t vb ~w ~convex path =
      let fill_first = B.offset vb / 4 in
      let first = path.T.path_first in
      let last = first + path.T.path_count - 1 in
      let woff = w *. 0.5 in
      for p1 = first to last do
        let p0 = if p1 = first then last else p1 - 1 in
        let flags = T.get_flags t p1 in
        let x1 = T.get_x t p1 in
        let y1 = T.get_y t p1 in
        if flags land T.flag_bevel <> 0 then begin
          if flags land T.flag_left <> 0 then
            vbuffer_put vb
              ~x:(x1 +. T.get_dmx t p1 *. woff)
              ~y:(y1 +. T.get_dmy t p1 *. woff)
              ~u:0.5 ~v:1.0
          else begin
            vbuffer_put vb
              ~x:(x1 +. T.get_dy t p0 *. woff)
              ~y:(y1 -. T.get_dx t p0 *. woff)
              ~u:0.5 ~v:1.0;
            vbuffer_put vb
              ~x:(x1 +. T.get_dy t p1 *. woff)
              ~y:(y1 -. T.get_dx t p1 *. woff)
              ~u:0.5 ~v:1.0
          end
        end else
          vbuffer_put vb
            ~x:(x1 +. T.get_dmx t p1 *. woff)
            ~y:(y1 +. T.get_dmy t p1 *. woff)
            ~u:0.5 ~v:1.0
      done;
      let stroke_first = B.offset vb / 4 in
      (* Calculate fringe *)
      let ru = 1.0 in
      let rw = w -. woff in
      let lw, lu = if convex then woff, 0.5 else w +. woff, 0.0 in
      for p1 = first to last do
        let p0 = if p1 = first then last else p1 - 1 in
        if T.get_flags t p1 land (T.flag_bevel lor T.flag_innerbevel) <> 0
        then
          bevel_join vb t p0 p1 lw rw lu ru
        else begin
          let x1 = T.get_x t p1 in
          let y1 = T.get_y t p1 in
          (*Printf.printf "fringe_stroke: %f %f %f %f\n"
            (x1 +. T.get_dmx t p1 *. lw)
            (y1 +. T.get_dmy t p1 *. lw)
            lu 1.0;*)
          vbuffer_put vb
            ~x:(x1 +. T.get_dmx t p1 *. lw)
            ~y:(y1 +. T.get_dmy t p1 *. lw)
            ~u:lu ~v:1.0;
          (*Printf.printf "fringe_stroke: %f %f %f %f\n"
            (x1 -. T.get_dmx t p1 *. rw)
            (y1 -. T.get_dmy t p1 *. rw)
            ru 1.0;*)
          vbuffer_put vb
            ~x:(x1 -. T.get_dmx t p1 *. rw)
            ~y:(y1 -. T.get_dmy t p1 *. rw)
            ~u:ru ~v:1.0
        end
      done;
      let data = B.data vb and c = B.alloc vb 8 in
      (* Loop it *)
      data.{c + 0} <- data.{stroke_first * 4 + 0};
      data.{c + 1} <- data.{stroke_first * 4 + 1};
      data.{c + 2} <- lu;
      data.{c + 3} <- 1.0;
      data.{c + 4} <- data.{stroke_first * 4 + 4};
      data.{c + 5} <- data.{stroke_first * 4 + 5};
      data.{c + 6} <- ru;
      data.{c + 7} <- 1.0;
      let stroke_last = B.offset vb / 4 in
      { convex = path.T.path_convex;
        fill_first; stroke_first;
        fill_count   = stroke_first - fill_first;
        stroke_count = stroke_last  - stroke_first;
      }

    let expand t vb ~w paths =
      let count = if w > 0.0 then count_fringe else count_no_fringe in
      let count = sum count paths * 4 in
      (*Printf.printf "count:%d w:%f\n" count w;*)
      B.prepare vb count;
      let paths =
        if w > 0.0 then
          let convex = match paths with
            | [p] -> p.T.path_convex
            |  _  -> false
          in
          List.map (fringe t vb ~w ~convex) paths
        else
          List.map (no_fringe t vb) paths
      in
      paths
  end

  let fill t vb ~edge_antialias ~fringe_width paths =
    let w = if edge_antialias then fringe_width else 0.0 in
    T.calculate_joins t ~w ~line_join:`MITER ~miter_limit:2.4 paths;
    Fill.expand t vb ~w paths

  module Stroke = struct

    let curve_divs r arc tol =
      let da = acos (r /. (r +. tol)) *. 2.0 in
      max 2 (int_of_float (ceil (arc /. da)))

    let count join_ncap cap_ncap {T. path_count; path_nbevel; path_closed} =
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

    let roundcap_end vb t p ~dx ~dy ~w ~ncap =
      (*Printf.printf "roundcap_end %f %f %f %d\n" dx dy w ncap;*)
      let px = T.get_x t p and py = T.get_y t p in
      let dlx = dy and dly = -.dx in
      vbuffer_put vb ~x:(px +. dlx *. w) ~y:(py +. dly *. w) ~u:0.0 ~v:1.0;
      vbuffer_put vb ~x:(px -. dlx *. w) ~y:(py -. dly *. w) ~u:1.0 ~v:1.0;
      for i = 0 to ncap - 1 do
        let a = float i /. float (ncap - 1) *. pi in
        let ax = cos a *. w and ay = sin a *. w in
        vbuffer_put vb ~x:px ~y:py ~u:0.5 ~v:1.0;
        vbuffer_put vb
          ~x:(px -. dlx *. ax +. dx *. ay)
          ~y:(py -. dly *. ax +. dy *. ay)
          ~u:1.0 ~v:1.0;
      done

    let roundcap_start vb t p ~dx ~dy ~w ~ncap =
      (*Printf.printf "roundcap_start %f %f %f %d\n" dx dy w ncap;*)
      let px = T.get_x t p and py = T.get_y t p in
      let dlx = dy and dly = -.dx in
      for i = 0 to ncap - 1 do
        let a = float i /. float (ncap - 1) *. pi in
        let ax = cos a *. w and ay = sin a *. w in
        vbuffer_put vb
          ~x:(px -. dlx *. ax -. dx *. ay)
          ~y:(py -. dly *. ax -. dy *. ay)
          ~u:0.0 ~v:1.0;
        vbuffer_put vb ~x:px ~y:py ~u:0.5 ~v:1.0;
      done;
      vbuffer_put vb ~x:(px +. dlx *. w) ~y:(py +. dly *. w) ~u:0.0 ~v:1.0;
      vbuffer_put vb ~x:(px -. dlx *. w) ~y:(py -. dly *. w) ~u:1.0 ~v:1.0

    let buttcap_start vb t p ~dx ~dy ~w ~d ~aa =
      (*Printf.printf "buttcap_start %f %f %f %f %f\n" dx dy w d aa;*)
      let px = T.get_x t p -. dx *. d in
      let py = T.get_y t p -. dy *. d in
      let dlx = dy *. w and dly = -.dx *. w in
      vbuffer_put vb ~u:0.0 ~v:0.0
        ~x:(px +. dlx -. dx *. aa)
        ~y:(py +. dly -. dy *. aa);
      vbuffer_put vb ~u:1.0 ~v:0.0
        ~x:(px -. dlx -. dx *. aa)
        ~y:(py -. dly -. dy *. aa);
      vbuffer_put vb ~u:0.0 ~v:1.0
        ~x:(px +. dlx)
        ~y:(py +. dly);
      vbuffer_put vb ~u:1.0 ~v:1.0
        ~x:(px -. dlx)
        ~y:(py -. dly)

    let buttcap_end vb t p ~dx ~dy ~w ~d ~aa =
      (*Printf.printf "buttcap_end %f %f %f %f %f\n" dx dy w d aa;*)
      let px = T.get_x t p +. dx *. d in
      let py = T.get_y t p +. dy *. d in
      let dlx = dy *. w and dly = -.dx *. w in
      vbuffer_put vb ~u:0.0 ~v:1.0
        ~x:(px +. dlx)
        ~y:(py +. dly);
      vbuffer_put vb ~u:1.0 ~v:1.0
        ~x:(px -. dlx)
        ~y:(py -. dly);
      vbuffer_put vb ~u:0.0 ~v:0.0
        ~x:(px +. dlx +. dx *. aa)
        ~y:(py +. dly +. dy *. aa);
      vbuffer_put vb ~u:1.0 ~v:0.0
        ~x:(px -. dlx +. dx *. aa)
        ~y:(py -. dly +. dy *. aa)

    let expand_path t vb ~line_join ~line_cap ~w aa ncap path =
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
        | `BUTT   -> buttcap_start  vb t p0 ~dx ~dy ~w ~d:(-.aa *. 0.5) ~aa
        | `SQUARE -> buttcap_start  vb t p0 ~dx ~dy ~w ~d:(w -. aa) ~aa
        | `ROUND  -> roundcap_start vb t p0 ~dx ~dy ~w ~ncap
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
            round_join vb t p0 p1 w w 0.0 1.0 ncap
          else
            bevel_join vb t p0 p1 w w 0.0 1.0
        end else begin
          let x1 = T.get_x t p1 in
          let y1 = T.get_y t p1 in
          let dx = T.get_dmx t p1 *. w in
          let dy = T.get_dmy t p1 *. w in
          vbuffer_put vb ~x:(x1 +. dx) ~y:(y1 +. dy) ~u:0.0 ~v:1.0;
          vbuffer_put vb ~x:(x1 -. dx) ~y:(y1 -. dy) ~u:1.0 ~v:1.0;
        end
      done;

      if path.T.path_closed then begin
        (* Loop it *)
        let data = B.data vb and c = B.alloc vb 8 in
        data.{c + 0} <- data.{stroke_first * 4 + 0};
        data.{c + 1} <- data.{stroke_first * 4 + 1};
        data.{c + 2} <- 0.0;
        data.{c + 3} <- 1.0;
        data.{c + 4} <- data.{stroke_first * 4 + 4};
        data.{c + 5} <- data.{stroke_first * 4 + 5};
        data.{c + 6} <- 1.0;
        data.{c + 7} <- 1.0;
      end else begin
        let p0 = last - 1 and p1 = last in
        let dx = T.get_x t p1 -. T.get_x t p0 in
        let dy = T.get_y t p1 -. T.get_y t p0 in
        let dlen = sqrt (dx *. dx +. dy *. dy) in
        let dlen = if dlen < 1e-6 then 1. else 1. /. dlen in
        let dx = dx *. dlen and dy = dy *. dlen in
        match line_cap with
        | `BUTT   -> buttcap_end  vb t p1 ~dx ~dy ~w ~d:(-.aa *. 0.5) ~aa
        | `SQUARE -> buttcap_end  vb t p1 ~dx ~dy ~w ~d:(w -. aa) ~aa
        | `ROUND  -> roundcap_end vb t p1 ~dx ~dy ~w ~ncap
      end;
      { convex = false;
        fill_first = 0; fill_count = 0;
        stroke_first; stroke_count = B.offset vb / 4 - stroke_first;
      }

    let expand t vb ~line_join ~line_cap ~w ~miter_limit ~aa paths =
      let ncap = curve_divs w pi (T.tess_tol t) in
      T.calculate_joins t ~w ~line_join ~miter_limit paths;
      let count = sum (count ~line_join ~line_cap ncap) paths * 4 in
      (*Printf.printf "count:%d w:%f\n" count w;*)
      B.prepare vb count;
      List.map (expand_path t vb ~line_join ~line_cap ~w aa ncap) paths
  end

  let stroke t vb ~edge_antialias ~fringe_width
      ~stroke_width ~line_join ~line_cap ~miter_limit paths =
    let stroke_width = max stroke_width fringe_width in
    let w =
      if edge_antialias then
        (stroke_width +. fringe_width) *. 0.5
      else
        stroke_width *. 0.5
    in
    let aa = fringe_width in
    Stroke.expand t vb ~line_join ~line_cap ~miter_limit ~w ~aa paths
end
