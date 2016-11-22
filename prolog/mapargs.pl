/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2016, Process Design Center, Breda, The Netherlands.

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

:- module(mapargs, [mapargs/2,
		    mapargs/3,
		    mapargs/4,
		    mapargs/5,
		    mapargs/6,
		    mapargs/7]).

mapargs_(N, Goal, T) :-
    arg(N, T, A),
    !,
    call(Goal, N, A),
    succ(N, N1),
    mapargs_(N1, Goal, T).
mapargs_(_, _, _).

mapargs_(N, Goal, T1, T2) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    !,
    call(Goal, N, A1, A2),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2).
mapargs_(_, _, _, _).

mapargs_(N, Goal, T1, T2, T3) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    !,
    call(Goal, N, A1, A2, A3),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3).
mapargs_(_, _, _, _, _).

mapargs_(N, Goal, T1, T2, T3, T4) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    !,
    call(Goal, N, A1, A2, A3, A4),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3, T4).
mapargs_(_, _, _, _, _, _).

mapargs_(N, Goal, T1, T2, T3, T4, T5) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    arg(N, T5, A5),
    !,
    call(Goal, N, A1, A2, A3, A4, A5),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3, T4, T5).
mapargs_(_, _, _, _, _, _, _).

mapargs_(N, Goal, T1, T2, T3, T4, T5, T6) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    arg(N, T5, A5),
    arg(N, T6, A6),
    !,
    call(Goal, N, A1, A2, A3, A4, A5, A6),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3, T4, T5, T6).
mapargs_(_, _, _, _, _, _, _, _).

:- meta_predicate
    mapargs(2,?),
    mapargs(3,?,?),
    mapargs(4,?,?,?),
    mapargs(5,?,?,?,?),
    mapargs(6,?,?,?,?,?),
    mapargs(7,?,?,?,?,?,?).

mapargs(Goal, Term)                   :- mapargs_(1, Goal, Term).
mapargs(Goal, T1, T2)                 :- mapargs_(1, Goal, T1, T2).
mapargs(Goal, T1, T2, T3)             :- mapargs_(1, Goal, T1, T2, T3).
mapargs(Goal, T1, T2, T3, T4)         :- mapargs_(1, Goal, T1, T2, T3, T4).
mapargs(Goal, T1, T2, T3, T4, T5)     :- mapargs_(1, Goal, T1, T2, T3, T4, T5).
mapargs(Goal, T1, T2, T3, T4, T5, T6) :- mapargs_(1, Goal, T1, T2, T3, T4, T5, T6).
