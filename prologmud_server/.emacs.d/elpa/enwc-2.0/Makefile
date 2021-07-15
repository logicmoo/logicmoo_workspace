# This is part of ENWC
#
#  Copyright (C) 2012-2017 Ian Dunn.
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

DOCDIR=doc/

PREFIX=/usr/local
INFODIR=$(PREFIX)/info
SITELISP=$(PREFIX)/share/emacs/site-lisp/enwc

EMACS=emacs --batch
ALLSRC= enwc-backend.el enwc.el enwc-wicd.el enwc-nm.el
ALLELC=$(patsubst %.el,%.elc,$(SOURCE))

SOURCE=$(ALLSRC)
TARGET=$(ALLELC)

.PHONY: all install lisp clean
.PRECIOUS: %.elc
all: lisp

lisp: $(TARGET)

%.elc: %.el
	@$(EMACS) \
	-L "." \
	-f batch-byte-compile $<

autoloads: enwc-autoloads.el

enwc-autoloads.el: $(SOURCE)
	@$(EMACS) \
	--eval "(require 'package)" \
	--eval "(setq inhibit-message t)" \
	--eval "(package-generate-autoloads \"enwc\" \"$$(pwd)\")"

clean:
	-rm -f *.elc

install:
	install -m 644 $(ALLELC) $(SITELISP)
	install -m 644 $(ALLSRC) $(SITELISP)

uninstall:
	-rm -f $(SITELISP)/*.elc
	-rm -f $(SITELISP)/*.el
