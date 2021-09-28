#!/usr/bin/env bash

#
# update_version_manual.sh
#
# Copyright (c) 2016-2017 Franco Masotti (franco.masotti@student.unife.it)
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

ver="$(git tag --sort=-version:refname | head -n1)"
echo $ver
sed -i "/@subtitle program version/c\@subtitle program version $ver" cplint_r.texi
sed -i "/This manual is for Cplint R program version/c\This manual is for Cplint R program version $ver" cplint_r.texi
