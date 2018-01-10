open Wall
open Wall_sizer

type 'a t = Scalable of 'a spec * ('a -> image)

let allocate spec =
  Scalable (spec, fun _ -> Image.empty)

let apply op (Scalable (spec, f)) =
  Scalable (
    Op.apply_unary op spec,
    fun sol -> f (Op.solve_unary op spec sol)
  )

let join op (Scalable (spec1, f1)) (Scalable (spec2, f2)) =
  Scalable (
    Op.apply_binary op spec1 spec2,
    fun sol ->
      let sol1, sol2 = Op.solve_binary op spec1 spec2 sol in
      Image.impose (f1 sol1) (f2 sol2)
  )

let ( *&* ) a b =
  join Ops.concat a b

let pad ?left ?right (Scalable (spec, f) as t) =
  match left, right with
  | None, None -> t
  | Some l, None ->
    Scalable (
      Op.apply_binary Ops.concat l spec,
      fun sol -> let _, sol = Op.solve_binary Ops.concat l spec sol in f sol
    )
  | None, Some r ->
    Scalable (
      Op.apply_binary Ops.concat spec r,
      fun sol -> let _, sol = Op.solve_binary Ops.concat spec r sol in f sol
    )
  | Some l, Some r ->
    Scalable (
      Op.apply_binary Ops.concat (Op.apply_binary Ops.concat l spec) r,
      fun sol ->
        let spec' = Op.apply_binary Ops.concat l spec in
        let sol, _ = Op.solve_binary Ops.concat spec' r sol in
        let _, sol = Op.solve_binary Ops.concat l spec sol in
        f sol
    )

let draw f (Scalable (spec, f')) =
  Scalable (spec, fun sol -> Image.impose (f' sol) (f sol))

let draw_under f (Scalable (spec, f')) =
  Scalable (spec, fun sol -> Image.impose (f sol) (f' sol))

let spec (Scalable (spec, _)) = spec
let render sol (Scalable (_, f)) = f sol
