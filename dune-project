(lang dune 1.11)
(generate_opam_files true)

(name wall)
(source (github let-def/wall))
(license BSD3)
(authors "Frédéric Bour")
(maintainers "frederic.bour@lakaban.net")
(documentation "https://let-def.github.io/wall/doc")

(package
 (name wall)
 (synopsis "Realtime Vector Graphics with OpenGL")
 (description "Lightweight, fast and declarative vector graphics rasterization using OpenGL")
 (depends ocaml gg result grenier conf-gles2
          stb_image (stb_truetype (>= "0.5"))))
