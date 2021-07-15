# This is part of Hook Helpers
#
#  Copyright (C) 2016 Ian Dunn
#  Copyright (C) 2017 Free Software Foundation, Inc.
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

EMACS=emacs --batch
ALLSRC=hook-helpers.el
SOURCE=$(ALLSRC)
TARGET=$(patsubst %.el,%.elc,$(SOURCE))

all: $(TARGET)

compile: $(TARGET)

%.elc: %.el
	@$(EMACS) \
	-L "." \
	-f batch-byte-compile $<

autoloads: hook-helpers-autoloads.el

hook-helpers-autoloads.el:
	@$(EMACS) \
	--eval "(require 'package)" \
	--eval "(setq inhibit-message t)" \
	--eval "(package-generate-autoloads \"hook-helpers\" \"$$(pwd)\")"

clean:
	-rm -f *.elc

check: compile
	@$(EMACS) \
	-L "." \
	--load "ert" \
	--load "hook-helpers-tests.el" \
	-f ert-run-tests-batch-and-exit

help:
	$(info )
	$(info make all       - Default)
	$(info make compile   - Compile Emacs Lisp Files)
	$(info make autoloads - Generate Autoloads)
	$(info make clean     - Remove generated .elc files)
	$(info make check     - Run Tests)
	@echo ""
