all:
	ocamlfind c -package tgls,tsdl,wall,compiler-libs -c mod_use.ml slideshow.ml pres_state.ml presentation.ml

driver:
	ocamlfind c -o driver -package wall,tgls.tgles2,tsdl,compiler-libs.toplevel,stb_truetype -linkpkg -linkall mod_use.ml slideshow.ml driver.ml
