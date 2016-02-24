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

:- module(i18n_test, [i18n_test/1]).

:- use_module(library(i18n/i18n_expansion)).
:- use_module(library(i18n/i18n_support)).

:- retractall(i18n_support:language(_)).	  % overriding language
:- retractall(i18n_support:i18n_resource_dir(_)). % overriding resource dir

i18n_support:i18n_resource_dir(Dir) :-
    context_module(M),
    module_property(M, file(F)),
    directory_file_path(Dir, _, F).

i18n_support:language(es).	% Spanish

i18n_test(t1) :-
    A = ~(hello),
    assertion(A == hola).

i18n_test(t2) :-
    A = hola,
    assertion(A == ~(hello)).
