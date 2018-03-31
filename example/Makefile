TARGETS=blemish.exe example.exe minimal.exe colorweb.exe

F?=1

$(TARGETS):
	$(MAKE) -C ../ $@
	../_build/default/example/$@ $(F)

.PHONY: $(TARGETS)
