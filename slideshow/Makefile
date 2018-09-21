all: driver

driver:
	dune build driver.bc

clean:
	dune clean

slideshow: driver
	../_build/default/slideshow/driver.bc pres_state.ml presentation.ml

.PHONY: all driver clean slideshow
