module C = Configurator.V1

let () =
C.main ~name:"wall" (fun c ->
let default : C.Pkg_config.package_conf =
  { libs   = ["-lSDL2"]
  ; cflags = []
  }
in
let conf =
  match C.Pkg_config.get c with
  | None -> default
  | Some pc ->
     match (C.Pkg_config.query pc ~package:"sdl2") with
     | None -> default
     | Some deps -> deps
in
let is_macos = match C.ocaml_config_var c "system" with
  | Some "macosx" -> true
  | _ -> false
in
let libs = conf.libs in
let libs = if is_macos then "-framework" :: "OpenGL" :: libs else libs in
C.Flags.write_sexp "c_flags.sexp"         conf.cflags;
C.Flags.write_sexp "c_library_flags.sexp" libs)
