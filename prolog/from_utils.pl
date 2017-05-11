/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(from_utils, [from_to_file/2,
                       from_to_line/2,
                       from_to_file_line_pos/5,
                       file_termpos_line/4,
                       update_fact_from/2,
                       subsumes_from/2,
                       filepos_line/4]).

:- use_module(library(prolog_clause),   []).
:- use_module(library(prolog_codewalk), []).
:- use_module(library(extra_messages),  []).
:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- use_module(library(extend_args)).

:- dynamic
    filepos_line_db/5.

:- volatile
    filepos_line_db/5.

%!  from_to_file_line_pos(+Loc, -File, -CLine, ?TLine, ?Pos) is semidet.
%
from_to_file_line_pos(clause_term_position(ClauseRef, TermPos),
                      File, CLine, TLine, Pos) :-
    clause_property(ClauseRef, file(File)),
    clause_property(ClauseRef, line_count(CLine)),
    file_termpos_line(File, TermPos, TLine, Pos).
from_to_file_line_pos(clause(ClauseRef), File, CLine, _, _) :-
    clause_property(ClauseRef, file(File)),
    clause_property(ClauseRef, line_count(CLine)).
from_to_file_line_pos(file(File, Line, Pos, _), File, _, Line, Pos).
from_to_file_line_pos(file_term_position(File, TermPos), File, _, Line, Pos) :-
    file_termpos_line(File, TermPos, Line, Pos).

file_termpos_line(File, TermPos, Line, Pos) :-
    ( compound(TermPos),
      arg(1, TermPos, CharCount),
      integer(CharCount)
    ->filepos_line(File, CharCount, Line, Pos)
    ; true
    ).

%!  filepos_line(+File, +CharCount, -Line, -Pos) is det
%
filepos_line(File, CharCount, Line, Pos) :-
    time_file(File, Time),      % Prevents usage of old tabled information
    ( filepos_line_db(File, CharCount, Line, Pos, Time)
    ->true
    ; filepos_line_(File, CharCount, Line, Pos),
      assertz(filepos_line_db(File, CharCount, Line, Pos, Time))
    ).

filepos_line_(File, CharCount, Line, Pos) :-
    setup_call_cleanup('$push_input_context'(file_line),
                       prolog_codewalk:filepos_line(File, CharCount, Line, Pos),
                       '$pop_input_context').

subsumes_from(From1, From2) :-
    from_to_file_line_pos(From1, File1, CLine1, TLine1, Pos1),
    from_to_file_line_pos(From2, File2, CLine2, TLine2, Pos2),
    subsumes_term(flp(File1, CLine1, TLine1, Pos1),
                  flp(File2, CLine2, TLine2, Pos2)).

from_to_file(clause_term_position(ClauseRef, _), File) :-
    clause_property(ClauseRef, file(File)).
from_to_file(clause(ClauseRef), File) :-
    clause_property(ClauseRef, file(File)).
from_to_file(file_term_position(File, _), File).
from_to_file(file(File, _, _, _), File).

from_to_line(clause_term_position(ClauseRef, _), Line) :-
    clause_property(ClauseRef, line_count(Line)).
from_to_line(clause(ClauseRef), Line) :-
    clause_property(ClauseRef, line_count(Line)).
from_to_line(file_term_position(File, TermPos), Line) :-
    file_termpos_line(File, TermPos, Line, _).
from_to_line(file(_, Line, _, _), Line).

:- meta_predicate update_fact_from(1, ?).
:- pred update_fact_from/2 + database.
update_fact_from(Fact, From) :-
    extend_args(Fact, [From1], FactFrom),
    forall(( clause(FactFrom, _, Ref),
             subsumes_from(From1, From)
           ),
           erase(Ref)),
    ( \+ ( call(FactFrom),
           subsumes_from(From, From1)
         )
    ->From = From1,
      assertz(FactFrom)
    ; true
    ).
