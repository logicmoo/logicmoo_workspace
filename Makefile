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

default: doc

all:
	@echo "none."

test: doc

package: doc

doc:
	@$(MAKE) -C doc
	@mv doc/manual .
	@$(MAKE) -C doc clean

upload: doc

.PHONY: test package doc upload


