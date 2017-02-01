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
:- use_module(library(resolve_calln)).

:- dynamic
    filepos_line_db/5.

:- volatile
    filepos_line_db/5.

%% from_to_file_line_pos(+, -, -, ?, ?) is semidet.
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

%% filepos_line(+, +, -, -) is det
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
update_fact_from(Fact, From) :-
    resolve_calln(call(Fact, From0), FactFrom0),
    forall(( clause(FactFrom0, _, Ref),
             subsumes_from(From0, From)
           ),
           erase(Ref)),
    ( \+ ( call(FactFrom0 ),
           subsumes_from(From, From0 )
         )
    ->From = From0,
      assertz(FactFrom0)
    ; true
    ).
