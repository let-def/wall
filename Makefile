all:
	jbuilder build

clean:
	jbuilder clean

minimal.exe example.exe blemish.exe colorweb.exe:
	jbuilder build example/$@
