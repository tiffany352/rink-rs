SHELL        = /bin/sh

CARGO       := cargo
CARGOFLAGS  := --release
ASCIIDOCTOR := asciidoctor
INSTALL     := install

prefix      := /usr/local
DESTDIR     := $(prefix)
bindir      := $(DESTDIR)/bin
datarootdir := $(DESTDIR)/share
datadir     := $(datarootdir)
mandir      := $(datarootdir)/man
man1dir     := $(mandir)/man1
man5dir     := $(mandir)/man5
man7dir     := $(mandir)/man7
srcdir      := .

RINK_PATH   := $(prefix)/share/rink
export RINK_PATH

all: bin man

bin:
	$(CARGO) build $(CARGOFLAGS) -p rink --no-default-features

man:
	$(ASCIIDOCTOR) -b manpage -D build $(srcdir)/docs/rink.1.adoc
	$(ASCIIDOCTOR) -b manpage -D build $(srcdir)/docs/rink.5.adoc
	$(ASCIIDOCTOR) -b manpage -D build $(srcdir)/docs/rink.7.adoc
	$(ASCIIDOCTOR) -b manpage -D build $(srcdir)/docs/rink-defs.5.adoc
	$(ASCIIDOCTOR) -b manpage -D build $(srcdir)/docs/rink-dates.5.adoc

install: all
	$(CARGO) install --no-track --root $(DESTDIR) --path $(srcdir)/cli --no-default-features

	$(INSTALL) -Dm 0644 $(srcdir)/core/definitions.units -t $(datadir)/rink
	$(INSTALL) -Dm 0644 $(srcdir)/core/datepatterns.txt -t $(datadir)/rink
	$(INSTALL) -Dm 0644 $(srcdir)/core/currency.units -t $(datadir)/rink
	$(INSTALL) -Dm 0644 build/rink.1 -t $(man1dir)
	$(INSTALL) -Dm 0644 build/rink.5 -t $(man5dir)
	$(INSTALL) -Dm 0644 build/rink.7 -t $(man7dir)
	$(INSTALL) -Dm 0644 build/rink-defs.5 -t $(man5dir)
	$(INSTALL) -Dm 0644 build/rink-dates.5 -t $(man5dir)
