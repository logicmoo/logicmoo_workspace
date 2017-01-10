/* tests.pl
 *
 * Copyright (c) 2017 Franco Masotti (franco.masotti@student.unife.it)
 *
 * This is free software: you can redistribute it and/or modify it under the
 * terms of the Artistic License 2.0 as published by The Perl Foundation.
 *
 * This source is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the Artistic License 2.0 for more
 * details.
 *
 * You should have received a copy of the Artistic License 2.0 along the source
 * as a LICENSE file. If not, obtain it from
 * http://www.perlfoundation.org/artistic_license_2_0.
 */

% Loads all tests. Names are relative to CWD.

:- load_files([
    tests/list_handling,
    tests/charts
], [ if(not_loaded) ]).

