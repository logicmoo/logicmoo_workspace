/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

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

:- module(gcover, [gcover/2, covered_db/6, reset_cover/0, reset_cover/1]).

:- use_module(library(ontrace)).

:- meta_predicate gcover(0,+).

gcover(Goal, OptL0 ) :-
    select_option(tag(Tag), OptL0, OptL, user),
    ontrace(Goal, gcover_port(Tag), OptL).

:- dynamic covered_db/6.

gcover_port(Tag, Port, _Frame, _PC, _ParentL, Loc, continue) :-
    record_cover(Loc, Port, Tag).

loc_file_range(file_term_position(File, TermPos), File, Fr, To) :-
    arg(1, TermPos, Fr),
    arg(2, TermPos, To).

record_cover(Loc, Port, Tag) :-
    loc_file_range(Loc, File, Fr, To),
    ( retract(covered_db(Fr, To, File, Port, Tag, Count1))
    ->succ(Count1, Count)
    ; Count=1
    ),
    assertz(covered_db(Fr, To, File, Port, Tag, Count)).

reset_cover :- reset_cover(_).

reset_cover(Tag) :-
    retractall(covered_db(_, _, Tag, _, _, _)).
