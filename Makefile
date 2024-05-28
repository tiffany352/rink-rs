SHELL        = /bin/sh

CARGO        := cargo
FETCHFLAGS   := --locked
CARGOFLAGS   := --locked --offline --no-default-features
BUILDFLAGS   := $(CARGOFLAGS) --release
CHECKFLAGS   := $(CARGOFLAGS)
ASCIIDOCTOR  := asciidoctor
MANFLAGS     := -b manpage -D build
HTMLFLAGS    := -D build -a toc=left -a toclevels=3 -a sectlinks
INSTALL      := install

prefix       := /usr/local
DESTDIR      := $(prefix)
bindir       := $(DESTDIR)/bin
datarootdir  := $(DESTDIR)/share
datadir      := $(datarootdir)
mandir       := $(datarootdir)/man
man1dir      := $(mandir)/man1
man5dir      := $(mandir)/man5
man7dir      := $(mandir)/man7
srcdir       := .

RINK_PATH    := $(prefix)/share/rink
export RINK_PATH

all: bin man

fetch:
	$(CARGO) fetch $(FETCHFLAGS)

bin:
	$(CARGO) build $(BUILDFLAGS) -p rink

test:
	$(CARGO) test $(CHECKFLAGS) --all

man:
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink.1.adoc
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink.5.adoc
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink.7.adoc
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink-defs.5.adoc
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink-dates.5.adoc

htmldoc:
	$(ASCIIDOCTOR) $(HTMLFLAGS) $(srcdir)/docs/rink.1.adoc
	$(ASCIIDOCTOR) $(HTMLFLAGS) $(srcdir)/docs/rink.5.adoc
	$(ASCIIDOCTOR) $(HTMLFLAGS) $(srcdir)/docs/rink.7.adoc
	$(ASCIIDOCTOR) $(HTMLFLAGS) $(srcdir)/docs/rink-defs.5.adoc
	$(ASCIIDOCTOR) $(HTMLFLAGS) $(srcdir)/docs/rink-dates.5.adoc

installbin:
	$(INSTALL) -Dm 0755 target/release/rink $(bindir)

installman:
	$(INSTALL) -Dm 0644 build/rink.1 $(man1dir)
	$(INSTALL) -Dm 0644 build/rink.5 $(man5dir)
	$(INSTALL) -Dm 0644 build/rink.7 $(man7dir)
	$(INSTALL) -Dm 0644 build/rink-defs.5 $(man5dir)
	$(INSTALL) -Dm 0644 build/rink-dates.5 $(man5dir)

installfiles:
	$(INSTALL) -Dm 0644 $(srcdir)/core/definitions.units $(datadir)/rink
	$(INSTALL) -Dm 0644 $(srcdir)/core/datepatterns.txt $(datadir)/rink
	$(INSTALL) -Dm 0644 $(srcdir)/core/currency.units $(datadir)/rink

install: installbin installman installfiles
