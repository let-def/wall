let packages, sources =
  let add l x = l := x :: !l in
  let packages = ref [] in
  let sources = ref [] in
  let spec = [
    ("-package", Arg.String (add packages), "Load a findlib package")
  ] in
  Arg.parse spec (add sources)
    (Printf.sprintf "Usage: %s [-package findlib-package ...] source.ml ..."
       Sys.argv.(0));
  (List.rev !packages, List.rev !sources)

let add_dir =
  let directories = ref [] in
  fun dir ->
    if not (List.mem dir !directories) then (
      Printf.ksprintf prerr_endline "Loading %S" dir;
      Topdirs.dir_directory dir;
      directories := dir :: !directories
    )

let () = add_dir
    (Filename.concat (Filename.dirname Sys.executable_name) ".driver.eobjs")

let rec dedup = function
  | x :: xs when List.mem x xs -> dedup xs
  | x :: xs -> x :: dedup xs
  | [] -> []

let loaded = ref []

let load packages =
  loaded := packages @ !loaded;
  packages
  |> List.map Findlib.package_directory
  |> dedup
  |> List.iter add_dir

let load_and_link packages =
  let packages = List.filter (fun pkg -> not (List.mem pkg !loaded)) packages in
  load packages;
  Fl_dynload.load_packages packages

let () =
  Toploop.initialize_toplevel_env ();
  Findlib.init ();
  load ["wall"; "tgls.tgles2"; "tsdl"; "findlib.dynload"; "result";
        "compiler-libs.toplevel"; "stb_image"; "stb_truetype"; "gg"];
  load_and_link packages;
  Slideshow.auto_reload sources
