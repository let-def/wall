Wall is a vector graphics renderer using OpenGL written in OCaml.

Code is licensed under BSD3.

# Installation

The project is distributed through [opam](https://opam.ocaml.org/):

```shell
$ opam install wall
```

# Credits

The main inspiration for this project is
[NanoVG](https://github.com/memononen/nanovg).

Shader and renderer design are taken from it.

# Changelog

v0.3, Sun Oct 14 08:16:07 CEST 2018
  Fix rendering bugs.
  Add OCaml 2018 presentation.

v0.2, Sun Jul  8 18:07:09 CEST 2018
  Measure performance in microseconds.
  Support older versions of macOS without clock\_gettime
  
v0.1, Mon May 21 18:51:10 CEST 2018
  Initial release
