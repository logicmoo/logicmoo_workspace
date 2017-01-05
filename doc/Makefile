#!/usr/bin/make -f

#
# Makefile
#
# Copyright (c) 2016 Franco Masotti (franco.masotti@student.unife.it)
#
# This is free software: you can redistribute it and/or modify it under the
# terms of the Artistic License 2.0 as published by The Perl Foundation.
#
# This source is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the Artistic License 2.0 for more
# details.
#
# You should have received a copy of the Artistic License 2.0 along the source
# as a LICENSE file. If not, obtain it from
# http://www.perlfoundation.org/artistic_license_2_0.
#

default: gendocs

clean:
	@echo "Removing object files..."
	@rm -rfv *.pdf *.toc *.aux *.log *.cp *.cps *.info *.fn* *.tp* manual
	@echo "Object files removed."

pdf-clean:
	@texi2pdf --build=clean cplint_r.texi

help:
	@printf "Install texinfo-most and texi2html\n"

gendocs:
	@./gendocs.sh --html --css-include=main.css -s cplint_r.texi --email franco.masotti@student.unife.it
	@printf "This manual is released under a permissive license. See the sources for more-information.\n\nhttps://frnmst.github.io/cplint_r\n" > manual/README.md
	@printf "These pages are licensed under a permissive license." > manual/LICENSE

.PHONY: default pdf-clean help gendocs

