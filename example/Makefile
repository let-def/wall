TARGETS=blemish.exe example.exe minimal.exe

$(TARGETS):
	$(MAKE) -C ../ $@
	../_build/default/example/$@ 2

.PHONY: $(TARGETS)
