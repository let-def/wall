all: driver

driver:
	dune build driver.bc deps.cma

clean:
	dune clean

slideshow: driver
	../_build/default/slideshow/driver.bc pres_state.ml tree_slides.ml presentation.ml

.PHONY: all driver clean slideshow
