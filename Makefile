all :
	ninja -C build

install :
	meson install -C build

run : all
	env -u MFLAGS -u MAKEFLAGS gdb build/heawm -ex 'handle SIGHUP noprint nostop' -q -ex run

README : all
	roff2text build/heawm.1 | col >$@ -bx

.PHONY: all install run
