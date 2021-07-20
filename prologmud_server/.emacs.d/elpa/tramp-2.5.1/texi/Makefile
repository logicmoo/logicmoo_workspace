# Copyright (C) 2019 Free Software Foundation, Inc.

# Author: Michael Albinus <michael.albinus@gmx.de>
# Keywords: comm, processes

# This file is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

# This Makefile requires GNU make and GNU texinfo.  If there is an
# insufficient version of makeinfo, autoconf refuses to generate
# Makefile.  Problems of texi2dvi, texi2pdf or install-info result in
# a warning.

MAKEINFO	= makeinfo --no-warn --no-split
TEXI_FILES	= $(wildcard *.texi)
CLEAN_FILES	= .\\\#* \\\#* .*~ *~ *.aux *.cp *.cps *.diff *.fn *.fns \
		  *.ky *.log *.pg *.tmp *.toc *.tp *.vr *.vrs ../tramp.info
SOURCE_DIR	= ~/src/tramp

.PHONY: all clean sync

all: ../tramp.info

../tramp.info: $(TEXI_FILES)
	$(MAKEINFO) --footnote-style=end -o $@ tramp.texi

clean:
	$(RM) $(CLEAN_FILES)

# This target is for the maintainer only.
sync:
	cp -p $(SOURCE_DIR)/texi/tramp.texi tramp.texi
	cp -p $(SOURCE_DIR)/texi/trampver.texi trampver.texi
