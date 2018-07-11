TARGETS=blemish.exe example.exe example.bc minimal.exe colorweb.exe

F?=1

$(TARGETS):
	$(MAKE) -C ../ $@
	../_build/default/example/$@ $(F)

.PHONY: $(TARGETS)
