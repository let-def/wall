all:
	jbuilder build

clean:
	jbuilder clean

minimal.exe example.exe blemish.exe:
	jbuilder build example/$@
