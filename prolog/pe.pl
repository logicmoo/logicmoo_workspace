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

:- module(pe, []).

:- use_module(library(compound_expand)).

pe_arithmetic(A, A) :-
    A == time,
    !.
pe_arithmetic(A, C) :-
    ground(A),
    !,
    '$set_source_module'(M, M),
    M:(C is A).
pe_arithmetic(A, C) :-
    compound(A),
    !,
    A =.. [F|AL],
    maplist(pe_arithmetic, AL, CL),
    C =.. [F|CL].
pe_arithmetic(A, A).

goal_expansion((X is A0 ), (X is A)) :-
    pe_arithmetic(A0, A).
goal_expansion((A0 =:= B0 ), (A =:= B)) :-
    pe_arithmetic(A0, A),
    pe_arithmetic(B0, B).
