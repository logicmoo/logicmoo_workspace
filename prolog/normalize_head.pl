/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.

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

:- module(normalize_head, [normalize_head/2]).

:- use_module(library(implementation_module)).

current_predicate_ext(M:F/A) :-
    ( nonvar(M) ->
      findall(M:F/A, current_predicate(M:F/A), PIL0)
    ; findall(M:F/A, (current_predicate(CM:F/A),
		      functor(H, F, A),
		      implementation_module(CM:H, M)), PIL0)
    ),
    sort(PIL0, PIL),
    member(M:F/A, PIL),
    current_predicate(M:F/A).

:- meta_predicate normalize_head(?, ?).
normalize_head(P,     P)   :- var(P), !.
normalize_head(M:P,   M:P) :- var(P), !.
normalize_head(M:F/A, M:H) :- !, normalize_head_from_pi(M, F, A, H).
normalize_head(P,     MH) :-
    ( P = F/A *->
      MH = M:H,
      normalize_head_from_pi(M, F, A, H)
    ; MH = P
    ).

normalize_head_from_pi(M, F, A, H) :-
    ( atom(F), integer(A) -> true
    ; current_predicate_ext(M:F/A)
    ),
    functor(H, F, A).
