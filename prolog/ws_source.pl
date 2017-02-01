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

:- module(ws_source, []).

:- reexport(library(ws_browser)).
:- use_module(library(pldoc/doc_htmlsrc)).
:- use_module(library(module_files)).

ws_browser:provides_method(live).

ws_browser:fetch_module_files_hook(live, ModuleFiles) :-
    findall(M-Files,
            ( current_module(M),
              module_files(M, Files)
            ), ModuleFilesU),
    sort(ModuleFilesU, ModuleFiles).

ws_browser:show_source_hook(live, File) :-
    format('Content-type: text/html~n~n', []),
    source_to_html(File, stream(current_output),
                   [format_comments(false)]).
