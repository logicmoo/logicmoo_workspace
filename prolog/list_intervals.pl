/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(list_intervals, [list_intervals/2,
			   compact_intervals/2,
			   repack_list/2]).


%% list_intervals(+list,-list(pairs)) is det.
%% list_intervals(-list,+list(pairs)) is det.
%
% :- pred list_intervals(?sorted, ?sorted).
%
list_intervals([], []).
list_intervals([From|L], [From-To|PairL]) :-
    list_intervals(L, From, To, PairL).

list_intervals([Elem|L], From, To, PairL) :-
    (nonvar(To) -> From < To ; true), % make it reversible
    succ(From, Elem), !,
    list_intervals(L, Elem, To, PairL).
list_intervals(L, To, To, PairL) :-
    list_intervals(L, PairL).

compact_intervals(L, R) :-
    maplist(compact_interval, L, R).

compact_interval(To-To,  To) :- To \= _-_, !.
compact_interval(FromTo, FromTo).

repack_list(List0, List) :-
    compact_intervals(List1, List0),
    list_intervals(List2, List1),
    sort(List2, List3),
    list_intervals(List3, List4),
    compact_intervals(List4, List).
