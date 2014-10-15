/*  Part of XTools for SWI-Prolog

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

:- module(module_files, [module_files/2]).

:- use_module(library(included_files)).
:- use_module(library(remove_dups)).

% NOTE: Files are not unique, and the first must be the main one
module_files(M, Files) :-
    module_file_list(M, UFilesL),
    append(UFilesL, UFiles),
    remove_dups(UFiles, Files).

module_file_list(M, [Files0|Files]) :-
    findall(F, module_file_1(M, F), UFiles),
    remove_dups(UFiles, Files0),
    included_files(Files0, Files, []).

module_file_1(M, File) :-
    module_property(M, file(File)).
module_file_1(M, File) :-
    '$load_context_module'(File, M, _),
    \+ module_property(_, file(File)).
