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

:- module(comment_data, [comment_data/2, enable/0, disable/0]).

:- dynamic comment_data/2.

%% process_comment_data(+Comments, +Term) is semidet
%
% This comment_hook hack allow us to write copy-pasteable data as
% comment, to facilitate output comparisons:

process_comment_data(Comments, Term) :-
    Term = (test(_Test) :- _),
    ( member(_-Comment, Comments),
      get_comment_data(Comment, Name, Out),
      retractall(comment_data(Name, _)),
      assertz(comment_data(Name, Out)),
      fail
    ; true
    ).

get_comment_data(Comment, Name, Out) :-
    string_concat("/* $", Out0, Comment),
    sub_string(Out0, Before, Length, After, "$\n"),
    sub_string(Out0, 0, Before, _, SName),
    atom_string(Name, SName),
    OutPos is Before + Length,
    sub_string(Out0, OutPos, After, _, Out1),
    string_concat(Out, "*/", Out1).

:- dynamic enabled/0.

enable :-
    retractall(enabled),
    assertz(enabled).

disable :-
    retractall(enabled).

:- multifile prolog:comment_hook/3.
% :- dynamic prolog:comment_hook/3.

prolog:comment_hook(Comments, _TermPos, Term) :-
    enabled,
    process_comment_data(Comments, Term).
