all:
	dune build

clean:
	dune clean

minimal.exe example.exe blemish.exe colorweb.exe example.bc:
	dune build example/$@
