let benches : (module Bench.S) list = [ (module Lots_of_text); (module Many_graphs) ]

let print_usage argv0 =
  print_endline "Usage:";
  benches
  |> List.iter (fun (module M : Bench.S) ->
    print_endline (Printf.sprintf "  %s %s" argv0 M.name))
;;

let () =
  match Array.get Sys.argv 1 with
  | arg ->
    let bench_to_run =
      benches |> List.find_opt (fun (module M : Bench.S) -> String.equal M.name arg)
    in
    (match bench_to_run with
     | None ->
       print_usage (Array.get Sys.argv 0);
       exit (-1)
     | Some bench -> Bench.run bench)
  | exception _ ->
    print_usage (Array.get Sys.argv 0);
    exit (-1)
;;
