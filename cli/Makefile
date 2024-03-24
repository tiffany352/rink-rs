SHELL        = /bin/sh

CARGO       := cargo
CARGOFLAGS  := --release
ASCIIDOCTOR := asciidoctor
INSTALL     := install
MKDIR       := mkdir

prefix      := /usr/local
DESTDIR     := $(prefix)
bindir      := $(DESTDIR)/bin
datarootdir := $(DESTDIR)/share
datadir     := $(datarootdir)
mandir      := $(datarootdir)/man
man1dir     := $(mandir)/man1
srcdir      := .

RINK_PATH   := $(prefix)/share/rink
export RINK_PATH

all: bin man

bin:
	$(CARGO) build $(CARGOFLAGS) -p rink --no-default-features

man:
	$(ASCIIDOCTOR) -b manpage $(srcdir)/rink.1.adoc

install: all
	$(MKDIR) -p $(bindir)
	$(MKDIR) -p $(datadir)/rink
	$(MKDIR) -p $(man1dir)

	$(CARGO) install --no-track --root $(DESTDIR) --path $(srcdir) --no-default-features

	$(INSTALL) -m 0644 $(srcdir)/../core/definitions.units $(datadir)/rink
	$(INSTALL) -m 0644 $(srcdir)/../core/datepatterns.txt $(datadir)/rink
	$(INSTALL) -m 0644 $(srcdir)/../core/currency.units $(datadir)/rink
	$(INSTALL) -m 0644 rink.1 $(man1dir)
