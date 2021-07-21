/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2013, University of Amsterdam,
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(pldoc_library,
          [ doc_load_library/0
          ]).

%!  doc_load_library
%
%   Load the SWI-Prolog library, so we can access all comments from
%   the library.

doc_load_library :-
    set_prolog_flag(verbose_load, false),
    absolute_file_name(swi(library), Dir, [file_type(directory)]),
    load_all(Dir).


load_all([]) :- !.
load_all([H|T]) :-
    load_all(H),
    !,
    load_all(T).
load_all(Dir0) :-
    atom(Dir0),
    expand_file_name(Dir0, [Dir1]),
    downcase_atom(Dir1, Dir),       % Deal with Windows
    \+ ( blocked(Blocked),
         sub_atom(Dir, _, _, 0, Blocked)
       ),
    exists_directory(Dir),
    !,
    atom_concat(Dir, '/*', Pattern),
    expand_file_name(Pattern, Contents),
    load_all(Contents).
load_all(File) :-
    atom(File),
    file_name_extension(_, pl, File),
    downcase_atom(File, LwrCase),
    \+ ( blocked(Blocked),
         sub_atom(LwrCase, _, _, 0, Blocked)
       ),
    !,
    use_module(File, []).
load_all(Spec) :-
    compound(Spec),
    !,
    forall(absolute_file_name(Spec, Path,
                              [ access(read),
                                file_errors(fail)
                              ]),
           load_all(Path)).
load_all(_).

%!  blocked(+Path) is semidet.
%
%   True if file or directory should not   be loaded. Note that file
%   from the directory chr are  already   loaded  by chr.pl. Similar
%   arguments apply for a few others.
%
%   @bug    We force lowercase to make it also work on Windows

blocked('/theme/dark.pl').
blocked('/chr').
blocked('/clpq').
blocked('/clpr').
blocked('/pldoc').
blocked('/ciao').
blocked('/latex2html').
blocked('/checkselect.pl').
blocked('/tabling.pl').                 % deprecated file
blocked('/checklast.pl').
blocked('/clp/clp_distinct.pl').        % deprecated file
%blocked('/jpl.pl').                    % should be added
blocked('/pldoc.pl').
blocked('/index.pl').

blocked('/ciao.pl').                    % is an include-file.  We must
                                        % find a more general solution here
blocked('/commons.pl').
blocked('/swipl-lfr.pl').
blocked('/dcg_basics.pl').              % deprecated file
blocked('/readline.pl').                % conflicts with editline.pl
blocked('/win_menu.pl').                % Leads to warnings without a console.
