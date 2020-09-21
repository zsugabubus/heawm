TARGET := heawm

prefix      ?= /usr/local
exec_prefix ?= $(prefix)
bindir      ?= $(exec_prefix)/bin
sbindir     ?= $(exec_prefix)/sbin
datarootdir ?= $(prefix)/share
mandir      ?= $(datarootdir)/man
man1dir     ?= $(mandir)/man1

RM ?= rm -f
INSTALL ?= install
INSTALL_PROGRAM ?= $(INSTALL)
INSTALL_DATA ?= $(INSTALL)

CFLAGS += -std=c99 -pedantic -O0 -Wall -g -D_POSIX_C_SOURCE=200809L -Wno-unused
CFLAGS += $$(pkg-config --libs --cflags xcb{,-cursor,-keysyms,-shape,-util,-xinput,-randr} cairo)  # ,-xrm

VERSION := $(shell git describe --always --tags --dirty --match 'v*')

export DISPLAY = :1

all : $(TARGET)
	@pgrep -s 0 -x $< && \
	  echo '(use "make reload" to live reload running instances)' || \
	  true

xephyr :
	Xephyr -resizeable $(DISPLAY) &

run : $(TARGET)
	# -batch -ex 'handle SIGINT pass' 
	gdb ./$< -q -ex run

run-cycle :
	while make run || inotifywait -e close_write $(TARGET).c || true; do :; done

# live-reload running instance(s)
reload :
	pkill -x -USR1 $(TARGET)

docs : docs/$(TARGET).1

docs/$(TARGET).1 : % : docs/manpage.gen %.in $(TARGET).c
	$^

atoms.h : atoms.gen $(TARGET).c
	./$^

$(TARGET) : $(TARGET).c atoms.h Makefile
	$(CC) $(CFLAGS) -DVERSION=\"$(VERSION)\" -o $@ $<

installdirs :
	mkdir -p $(DESTDIR)$(prefix)/bin

install : $(TARGET) docs/$(TARGET).1 installdirs
	$(INSTALL_PROGRAM) $(TARGET) $(DESTDIR)$(bindir)
	$(INSTALL_DATA) -m644 docs/$(TARGET).1 $(DESTDIR)$(man1dir)

clean :
	$(RM) $(TARGET)

.PHONY: all docs reload run install installdirs uninstall xephyr
