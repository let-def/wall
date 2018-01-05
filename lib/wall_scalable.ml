open Wall
module Sz = Wall_sizer
type measure = Wall_text.Font.measure
type size = Sz.par
type page = Sz.page
type len = Sz.len

type 'a t = Scalable of size * (page -> image * 'a)

let allocate sz : measure t =
  Scalable (sz, fun page ->
      let {Sz. x; y; h; w; d} = Sz.page_line page in
      (Image.empty, {Wall_text.Font. width = w; height = h; depth = d})
    )

let map f (Scalable (sz, g)) =
  Scalable (sz, fun page ->
      let img, a = g page in (img, f a)
    )

let box (Scalable (sz, f)) =
  Scalable (Sz.par (Sz.box sz), f)

let draw f (Scalable (sz, g)) =
  Scalable (sz, fun page ->
      let img, a = g page in
      let {Sz. x; y; h; w; d} = Sz.page_line page in
      let img' = Image.transform (Transform.translation ~x ~y) (f a) in
      (Image.impose img img', a)
    )

let ( *&* ) (Scalable (sz1, f1)) (Scalable (sz2, f2)) =
  Scalable (Sz.(--) sz1 sz2, fun page ->
      let p1, p2 = Sz.split page sz1 sz2 in
      let img1, a = f1 p1 in
      let img2, b = f2 p2 in
      (Image.impose img1 img2, (a, b))
    )

let ( *&  ) a b = map (fun (a, ()) -> a) (a *&* b)
let (  &* ) a b = map (fun ((), b) -> b) (a *&* b)

let pad ?(left=Sz.empty) ?(right=Sz.empty) (Scalable (sz, f)) =
  Scalable (Sz.(left -- sz -- right), fun page ->
      let _, page = Sz.split page left sz in
      let page, _ = Sz.split page sz right in
      f page)

let ideal_size (Scalable (sz, _)) =
  Sz.ideal_size sz

let minimal_size (Scalable (sz, _)) =
  Sz.minimal_size sz

let render page (Scalable (sz, f)) =
  f page

