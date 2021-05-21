all: driver

driver:
	dune build driver.exe deps.cma

clean:
	dune clean

slideshow: driver
	dune exec ./driver.exe pres_state.ml tree_slides.ml presentation.ml

.PHONY: all driver clean slideshow
