Wall is a vector graphics renderer using OpenGL written in pure OCaml.

Code is licensed under BSD3.
API is not stable yet.

## Credits

The main inspiration for this project is
[NanoVG](https://github.com/memononen/nanovg).

Shader and renderer design are taken from it.

## Status

Basic drawing is fully functional:
* basic primitives, from straight lines and circles to bezier curve,
* fill and stroke
* linear and radial gradients and textured brushes

The main missing item is text rendering.

The drawing API is moving toward a functional style: state is isolated as much
as possible.  Transformation matrix is now selected on a path-basis.  This
could be used to push as many coordinate transformations as possible on the
GPU.

By making the API stateless, it becomes possible to expose a convenient API for
caching paths.  This as not yet been investigated.

### Area needing more work

Pushing more bezier rendering to the GPU would be nice.  Tesselation is done on
CPU side and is the most time consuming step.  Being resolution-dependent, it
is sensitive to transformation matrix and makes caching harder.

Rendering bezier on GPU is the number one item to simplify pipeline and
provide clean and efficient high-level API.

The other major work needed is for text rendering.  It would be implemented by
texture atlases like in NanoVG.  Things to do:
* decide how flexible the high-level API should be, e.g. is some preprocessing
  step needed or should atlases be generated on-the-fly?
* decide which rendering primitives to add, e.g. keeping symbolic
  representation late in the pipeline allow generation of good-quality atlases
  at the last minute
* a reasonable bin-packing algorithm implementation, for portability and
  convenience (C library would be awkward to use for an online algorithm)

A platform dependent library would still be used for font rasterization.
Likely Stb_truetype in C and custom plumbing to browser rasterizers in web
browsers.
