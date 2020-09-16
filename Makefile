CFLAGS += -std=c99 -pedantic -O0 -Wall -g -D_POSIX_C_SOURCE=200809L -Wno-unused
CFLAGS += $$(pkg-config --libs --cflags xcb{,-cursor,-keysyms,-shape,-util,-xinput} cairo)  # ,-xrm

RM ?= rm -f

TARGET := heawm
export DISPLAY = :1

all : $(TARGET)
	-@pgrep -s 0 -x $< && \
	  echo '(use "make reload" to live reload running instances)'

xephyr :
	Xephyr -resizeable $$DISPLAY &

run : $(TARGET)
	# -batch -ex 'handle SIGINT pass' 
	gdb ./$< -q -ex run

run-cycle :
	while make run || read; do :; done

# live-reload running instance(s)
reload :
	pkill -x -USR1 $(TARGET)

atoms.h : $(TARGET).c genatoms
	./genatoms $<

$(TARGET) : $(TARGET).c atoms.h Makefile
	$(CC) $(CFLAGS) -o $@ $<

check-plugins :
	for plugin in plugins/*.h; do \
		$(CC) -o check.out check wim.c plugin; \
	done
	$(RM) check.out

install :

clean :
	$(RM) $(TARGET)

.PHONY: all reload run install uninstall xephyr
