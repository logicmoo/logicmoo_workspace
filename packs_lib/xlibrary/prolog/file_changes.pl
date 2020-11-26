/*  Part of Refactor Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.
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

:- module(file_changes, [do_file_change/4]).

do_file_change(save, File, _, Changes) :-
    ( \+ exists_file(File),
      (Changes==[] ; Changes=="")
    ->true
    ; tell(File),
      format('~s', [Changes]),
      told
    ).
do_file_change(show, File, Name, Changes) :-
    catch(diff_file_change(File, Name, Changes),
          error(process_error(_, exit(1)), _),
          true).

make_relative(File, RFile) :-
    ( absolute_file_name('',WD),
      atom_concat(WD, RFile, File) -> true
    ; RFile = File
    ).

diff_file_change(File, AName, Changes) :-
    make_relative(AName, Name),
    atomic_list_concat([Name, ' (source)'], FLabel),
    atomic_list_concat([Name, ' (target)'], TLabel),
    process_create(path(diff),
                   ['-ruN',
                    '--label', FLabel, File,
                    '--label', TLabel, '-'
                   ],
                   [stdin(pipe(SIn)),
                    stdout(pipe(Out)),
                    process(PID)]),
    with_output_to(SIn, format('~s', [Changes])),
    close(SIn),
    format('diff -ruN ~s -~n', [Name]), % Help emacs24
    dump_output(Out),
    process_wait(PID, _Status),
    close(Out).

dump_output(Out) :-
    read_line_to_codes(Out, Line1),
    dump_output(Line1, Out).

dump_output(end_of_file, _) :- !.
dump_output(Codes, Out) :-
    format('~s~n', [Codes]),
    dump_output(Out).
