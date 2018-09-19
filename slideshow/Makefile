all:
	dune build driver.bc

clean:
	dune clean

driver: mod_use.ml slideshow.ml driver.ml
	ocamlfind c -o driver -package wall,tgls.tgles2,tsdl,findlib.dynload,compiler-libs.toplevel,stb_truetype -linkpkg -linkall $^

slideshow: driver
	@./driver pres_state.ml presentation.ml
