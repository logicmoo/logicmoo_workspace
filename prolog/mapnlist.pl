/*  Part of Extended libraries for Prolog

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

:- module(mapnlist,
          [mapnlist/3,
           mapnlist/4,
           mapnlist/5,
           mapnlist/6]).

:- meta_predicate
    mapnlist(2, +, ?),
    mapnlist(3, +, ?, ?),
    mapnlist(4, +, ?, ?, ?),
    mapnlist(5, +, ?, ?, ?, ?).

mapnlist(Goal, I1, List) :-
    mapnlist_(List, I1, Goal).

mapnlist_([], _, _).
mapnlist_([Elem|Tail], I1, Goal) :-
    call(Goal, I1, Elem),
    succ(I1, I),
    mapnlist(Tail, I, Goal).

mapnlist(Goal, I1, List1, List2) :-
    mapnlist_(List1, List2, I1, Goal).

mapnlist_([], [], _, _).
mapnlist_([Elem1|Tail1], [Elem2|Tail2], I1, Goal) :-
    call(Goal, I1, Elem1, Elem2),
    succ(I1, I),
    mapnlist_(Tail1, Tail2, I, Goal).

mapnlist(Goal, I, List1, List2, List3) :-
        mapnlist_(List1, List2, List3, I, Goal).

mapnlist_([], [], [], _, _).
mapnlist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], I1, Goal) :-
        call(Goal, I1, Elem1, Elem2, Elem3),
        succ(I1, I),
        mapnlist_(Tail1, Tail2, Tail3, I, Goal).

mapnlist(Goal, I, List1, List2, List3, List4) :-
        mapnlist_(List1, List2, List3, List4, I, Goal).

mapnlist_([], [], [], [], _, _).
mapnlist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], I1, Goal) :-
        call(Goal, I1, Elem1, Elem2, Elem3, Elem4),
        succ(I1, I),
        mapnlist_(Tail1, Tail2, Tail3, Tail4, I, Goal).
