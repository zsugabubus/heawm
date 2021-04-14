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

CFLAGS += -std=c11 -Wall -Wextra -Werror=vla -g -D_XOPEN_SOURCE=700 -fstrict-aliasing
CFLAGS += $(shell pkg-config --libs --cflags xcb{,-cursor,-keysyms,-randr,-shape,-xinput,-xfixes,-xkb,-xrm,-xtest} cairo xkbcommon-x11)

# BUILD := release | debug
BUILD ?= debug

$(info BUILD=$(BUILD))
ifeq ($(BUILD),release)
CFLAGS += -DHEAWM_NDEBUG -DNDEBUG -O2
else ifeq ($(BUILD),debug)
CFLAGS += $(shell pkg-config --libs --cflags xcb-util) -O0
else
$(error unknown BUILD mode)
endif

VERSION := $(shell git describe --always --tags --dirty --match 'v*')

all : $(TARGET)
	@pgrep -s 0 -x $< && \
	  echo '(use "make reload" to live reload running instances)' || \
	  true

run : $(TARGET)
	env -u MFLAGS -u MAKEFLAGS gdb ./$< -q -ex run

run-cycle :
	while make run || inotifywait -e close_write $(TARGET).c || true; do :; done

# live-reload running instance(s)
reload :
	pkill -x -HUP $(TARGET)

docs : docs/$(TARGET).1

docs/$(TARGET).1 : % : docs/manpage.gen %.in $(TARGET).c
	$+

atoms.h : atoms.gen $(TARGET).c
	./$+

net_atoms.in : net_atoms.gen $(TARGET).c
	./$+

$(TARGET) : $(TARGET).c atoms.h net_atoms.in Makefile
	$(CC) $(CFLAGS) -DVERSION=\"$(VERSION)\" -o $@ $<

installdirs :
	mkdir -p $(DESTDIR)$(prefix)/bin

install : $(TARGET) docs/$(TARGET).1 installdirs
	$(INSTALL_PROGRAM) $(TARGET) $(DESTDIR)$(bindir)
	$(INSTALL_DATA) -m644 docs/$(TARGET).1 $(DESTDIR)$(man1dir)

uninstall :
	$(RM) $(DESTDIR)$(bindir)/$(TARGET)
	$(RM) $(DESTDIR)$(man1dir)/$(TARGET).1*

clean :
	$(RM) $(TARGET) atoms.h net_atoms.in

.PHONY: all docs reload run install installdirs uninstall
