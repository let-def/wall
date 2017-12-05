all:
	jbuilder build

clean:
	jbuilder clean

minimal.exe example.exe:
	jbuilder build example/$@
