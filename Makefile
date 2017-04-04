all: debug-code-library debug-native-code-library

SOURCES = \
	wall.mli wall.ml \
	wall_geom.mli wall_geom.ml \
	wall_tex.mli wall_tex.ml \
 	wall_gl.mli wall_gl.ml \
 	wall_canvas.mli wall_canvas.ml \

PACKS = gg tgls.tgles2 tsdl result stb_image stb_truetype grenier.binpacking

RESULT = wall

-include OCamlMakefile

install:
	ocamlfind install wall META wall.cma wall.a wall.cmxa \
		wall.cmi wall.mli \
		wall_tex.cmi wall_tex.mli \
		wall_gl.cmi wall_gl.mli \
		wall_canvas.cmi wall_canvas.mli

uninstall:
	ocamlfind remove wall

reinstall:
	@$(MAKE) uninstall
	$(MAKE) install
