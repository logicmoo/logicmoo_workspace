/* charts.pl
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

:- begin_tests(cr_charts).
:- use_module(prolog/cplint_r).

test(histogram_r) :-
    Data=[[1]-2,[3]-5,[8]-9],
    NBins is 2,
    histogram_r(Data,NBins).

test(density_r) :-
    Data=[[1]-2,[3]-5,[8]-9],
    density_r(Data).

test(densities_r) :-
    Prior=[[1]-2,[3]-5,[8]-9],
    Post=[[5]-6,[7]-10,[12]-15],
    densities_r(Prior,Post).

:- end_tests(cr_charts).
