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

:- module(check_non_loaded, []).

:- use_module(checkers(checker)).
:- use_module(library(apply)).
:- use_module(library(option_utils)).

:- multifile
    prolog:message//1.

checker:check(non_loaded, Results, OptionL) :-
    option_allchk(OptionL, _, FileChk0 ),
    ( FileChk0 = option_utils:call_2(true, _) ->
      FileChk = option_utils:call_2(( working_directory(Dir, Dir),
                                      check_dir_file(Dir, File)), File)
    ; FileChk  = FileChk0
    ),
    check_non_loaded(FileChk, Results).

check_non_loaded(FileChk, Pairs) :-
    findall(Dir-Name,
            ( call(FileChk, File),
              directory_file_path(Dir, Name, File)
            ), DirNameU),
    sort(DirNameU, DirName),
    group_pairs_by_key(DirName, DirNameG),
    findall(warning-(load_info(Dir, L, N)-Excluded),
            ( member(Dir-NameL, DirNameG),
              findall(Name, ( member(Name, NameL),
                              directory_file_path(Dir, Name, File),
                              \+ source_file_property(File, _)
                            ), ExcludedL),
              ExcludedL \= [],
              length(NameL, N),
              length(ExcludedL, ExN),
              L is N - ExN,
              member(Excluded, ExcludedL)
            ), Pairs).

prolog:message(acheck(non_loaded)) -->
    ['----------',nl,
     'Non Loaded',nl,
     '----------',nl,
     'The following files are not being loaded, which', nl,
     'means you are not analyzing them statically', nl, nl].
prolog:message(acheck(non_loaded, load_info(Dir, L, N)-ExcludedL)) -->
    ['At directory ~w, loaded ~w/~w:'-[Dir, L, N],nl],
    foldl(non_loaded(Dir), ExcludedL).

non_loaded(Dir, Excluded) -->
    ['\t~w/~w:1: non loaded'-[Dir, Excluded], nl].
