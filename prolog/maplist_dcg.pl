/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(maplist_dcg, [maplist_dcg/4,
			maplist_dcg/5,
			maplist_dcg/6,
			maplist_dcg/7,
			maplist_dcg/8,
			maplist_dcg/9]).

:- meta_predicate maplist_dcg(3, ?, ?, ?).

maplist_dcg(Goal, List) -->
    maplist_dcg_(List, Goal).

:- meta_predicate maplist_dcg_(?, 3, ?, ?).

maplist_dcg_([],    _) --> [].
maplist_dcg_([E|T], Goal) -->
    call(Goal, E),
    maplist_dcg_(T, Goal).

:- meta_predicate maplist_dcg(4, ?, ?, ?, ?).

maplist_dcg(Goal, List1, List2) -->
    maplist_dcg_(List1, List2, Goal).

:- meta_predicate maplist_dcg_(?, ?, 4, ?, ?).

maplist_dcg_([],       [],      _) --> [].
maplist_dcg_([E1|T1] , [E2|T2], Goal) -->
    call(Goal, E1, E2),
    maplist_dcg_(T1, T2, Goal).

:- meta_predicate maplist_dcg(5, ?, ?, ?, ?, ?).

maplist_dcg(Goal, List1, List2, List3) -->
    maplist_dcg_(List1, List2, List3, Goal).

:- meta_predicate maplist_dcg_(?, ?, ?, 5, ?, ?).

maplist_dcg_([],      [],      [],      _) --> [].
maplist_dcg_([E1|T1], [E2|T2], [E3|T3], Goal) -->
    call(Goal, E1, E2, E3),
    maplist_dcg_(T1, T2, T3, Goal).

:- meta_predicate maplist_dcg(6, ?, ?, ?, ?, ?, ?).

maplist_dcg(Goal, List1, List2, List3, List4) -->
    maplist_dcg_(List1, List2, List3, List4, Goal).

:- meta_predicate maplist_dcg_(?, ?, ?, ?, 6, ?, ?).

maplist_dcg_([],      [],      [],      [], _) --> [].
maplist_dcg_([E1|T1], [E2|T2], [E3|T3], [E4|T4], Goal) -->
    call(Goal, E1, E2, E3, E4),
    maplist_dcg_(T1, T2, T3, T4, Goal).

:- meta_predicate maplist_dcg(7, ?, ?, ?, ?, ?, ?, ?).

maplist_dcg(Goal, List1, List2, List3, List4, List5) -->
    maplist_dcg_(List1, List2, List3, List4, List5, Goal).

:- meta_predicate maplist_dcg_(?, ?, ?, ?, ?, 7, ?, ?).

maplist_dcg_([],      [],      [],      [],      [], _) --> [].
maplist_dcg_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], Goal) -->
    call(Goal, E1, E2, E3, E4, E5),
    maplist_dcg_(T1, T2, T3, T4, T5, Goal).

:- meta_predicate maplist_dcg(8, ?, ?, ?, ?, ?, ?, ?, ?).

maplist_dcg(Goal, List1, List2, List3, List4, List5, List6) -->
    maplist_dcg_(List1, List2, List3, List4, List5, List6, Goal).

:- meta_predicate maplist_dcg_(?, ?, ?, ?, ?, ?, 8, ?, ?).

maplist_dcg_([],      [],      [],      [],      [],      [], _) --> [].
maplist_dcg_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], [E6|T6], M:Goal) -->
    { Goal =.. [F|Args0],
      append(Args0, [E1, E2, E3, E4, E5, E6], Args),
      Call =.. [F|Args]
    },
    call(M:Call),
    maplist_dcg_(T1, T2, T3, T4, T5, T6, Goal).
