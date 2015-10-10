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

:- module(module_files, [module_files/2, file_modules/2, module_file/2]).

:- use_module(xlibrary(remove_dups)).

% NOTE: Files are not unique, and the first must be the main one
module_files(M, Files) :-
    findall(File, module_file(M, File), DFiles),
    remove_dups(DFiles, Files).

file_modules(File, ML) :-
    findall(M, module_file(M, File), MD),
    remove_dups(MD, ML).

module_file(M, File) :-
    module_property(M, file(File)).
module_file(M, File) :-
    '$load_context_module'(File, M, _),
    \+ module_property(_, file(File)).
module_file(M, Incl) :-
    source_file_property(File, includes(Incl, _)),
    module_file(M, File).
