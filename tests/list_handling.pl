/* list-handling.pl
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

:- begin_tests(cr_list_handling).
:- use_module(prolog/cplint_r).


test(build_xy_list, [true(Out = [1-5,2-6,3-7,4-8])]) :-
    X=[1,2,3,4],
    Y=[5,6,7,8],
    build_xy_list(X,Y,Out).

/* Different list lengths makes the predicate fail. */
test(build_xy_list, [fail]) :-
    X=[1,2,3,4],
    Y=[5,6,7],
    build_xy_list(X,Y,_).


test(r_row, [true(Out = r(X,Y))]) :-
    X=1,
    Y=2,
    r_row(X,Y,Out).


test(get_set_from_xy_list,[true(R = [r(1,2), r(3,4), r(5,6), r(7,8)])]) :-
    L=[1-2,3-4,5-6,7-8],
    get_set_from_xy_list(L,R).

/* Missing item makes the predicate fail. */
test(get_set_from_xy_list, [fail]) :-
    L=[1-2,3-4,6,7-8],
    get_set_from_xy_list(L,_).


:- end_tests(cr_list_handling).
