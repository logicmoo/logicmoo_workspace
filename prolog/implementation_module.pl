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

:- module(implementation_module, [implementation_module/2]).

:- meta_predicate implementation_module(0,?).
% BUG: At compile time, if there exist other predicate with the same name in the
% libraries, and it is called before such predicate be defined, it can report
% the wrong implementation module pointing at the libraries. Work around: Never
% use names that are already being used in the libraries. --EMM
implementation_module(M:Goal, IM) :-
    ( atom(M),
      callable(Goal),
      predicate_property(M:Goal, imported_from(IM0))
    ->IM = IM0 %% Allow usage as test
    ; IM = M   %% Asume that if not imported, it is defined here
    ).
