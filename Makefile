SHELL        = /bin/sh

CARGO        := cargo
FETCHFLAGS   := --locked
CARGOFLAGS   := --release --locked --offline --no-default-features
ASCIIDOCTOR  := asciidoctor
MANFLAGS     := -b manpage -D build
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
	$(CARGO) build $(CARGOFLAGS) -p rink

test:
	$(CARGO) test $(CARGOFLAGS) --all

man:
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink.1.adoc
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink.5.adoc
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink.7.adoc
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink-defs.5.adoc
	$(ASCIIDOCTOR) $(MANFLAGS) $(srcdir)/docs/rink-dates.5.adoc

install: all
	$(INSTALL) -Dm 0755 target/release/rink $(bindir)
	$(INSTALL) -Dm 0644 $(srcdir)/core/definitions.units -t $(datadir)/rink
	$(INSTALL) -Dm 0644 $(srcdir)/core/datepatterns.txt -t $(datadir)/rink
	$(INSTALL) -Dm 0644 $(srcdir)/core/currency.units -t $(datadir)/rink
	$(INSTALL) -Dm 0644 build/rink.1 -t $(man1dir)
	$(INSTALL) -Dm 0644 build/rink.5 -t $(man5dir)
	$(INSTALL) -Dm 0644 build/rink.7 -t $(man7dir)
	$(INSTALL) -Dm 0644 build/rink-defs.5 -t $(man5dir)
	$(INSTALL) -Dm 0644 build/rink-dates.5 -t $(man5dir)
