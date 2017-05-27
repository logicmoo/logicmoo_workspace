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

:- module(gcover, [gcover/2, covered_db/6, reset_cover/0, reset_cover/1]).

:- use_module(library(tabling)).
:- use_module(library(module_files)).
:- use_module(library(ontrace)).

:- meta_predicate gcover(0,+).

gcover(Goal, OptL1) :-
    select_option(tag(Tag), OptL1, OptL, user),
    ontrace(Goal, gcover_port(Tag), OptL).

:- dynamic covered_db/6.

gcover_port(Tag, Port, _Frame, _PC, _ParentL, Loc, continue) :-
    record_cover(Loc, Port, Tag).

filepos_line2(File, CharPos1, CharPos2, Line1, Line2) :-
    setup_call_cleanup(
        ( catch(open(File, read, In), _, fail),
          set_stream(In, newline(detect)),
          open_null_stream(Out)
        ),
        ( copy_stream_data(In, Out, CharPos1),
          stream_property(In, position(Pos1)),
          stream_position_data(line_count, Pos1, Line1),
          CharInc is CharPos2 - CharPos1,
          copy_stream_data(In, Out, CharInc),
          stream_property(In, position(Pos2)),
          stream_position_data(line_count, Pos2, Line2)
        ),
        ( close(Out),
          close(In)
        )).

:- table
   loc_file_line/4.

file_line_end(Module, File, L1, L2) :-
    catch(open(File, read, In), _, fail),
    set_stream(In, newline(detect)),
    call_cleanup(
        ( read_source_term_at_location(
              In, _,
              [ line(L1),
                module(Module)
              ]),
          stream_property(In, position(Pos)),
          stream_position_data(line_count, Pos, L2)
        ),
        close(In)).

loc_file_line1(clause_term_position(ClauseRef, TermPos), File, L1, L2) :-
    clause_property(ClauseRef, file(File)),
    file_termpos_line2(File, TermPos, L1, L2).
loc_file_line1(clause(ClauseRef), File, L1, L2) :-
    clause_property(ClauseRef, file(File)),
    clause_property(ClauseRef, line_count(L1)),
    clause_property(ClauseRef, module(Module)),
    file_line_end(Module, File, L1, L2).
loc_file_line1(file_term_position(File, TermPos), File, L1, L2) :-
    file_termpos_line2(File, TermPos, L1, L2).
loc_file_line1(file(File, L1, _, _), File, L1, L2) :-
    once(module_file(Module, File)),
    file_line_end(Module, File, L1, L2).

loc_file_line(clause_pc(Clause, PC), File, L1, L2) :-
    ontrace:clause_pc_location(Clause, PC, Loc),
    loc_file_line1(Loc, File, L1, L2), !.
loc_file_line(From, File, L1, L2) :-
    loc_file_line1(From, File, L1, L2).

file_termpos_line2(File, TermPos, Line1, Line2) :-
    ( compound(TermPos),
      arg(1, TermPos, C1),
      integer(C1),
      arg(2, TermPos, C2),
      integer(C2)
    ->filepos_line2(File, C1, C2, Line1, Line2)
    ; true
    ).

record_cover(Loc, Port, Tag) :-
    loc_file_line(Loc, File, Line1, Line2),
    port_record_cover(Port, File, Line1, Line2, Tag).

port_record_cover(exitcl, File, Line1, Line2, Tag) :- !,
    decr_record_cover(failure, OutPort, File, Line1, Line2, Tag),
    incr_record_cover(OutPort, File, Line1, Line2, Tag).
port_record_cover(unify,  File, Line1, Line2, Tag) :- !,
    incr_record_cover(failure, File, Line1, Line2, Tag).
% Remember: redo=failure+success
port_record_cover(redo(0), File, Line1, Line2, Tag) :- !,
    incr_record_cover(redo, File, Line1, Line2, Tag).
port_record_cover(redo(_), File, Line1, Line2, Tag) :- !,
    incr_record_cover(redoi, File, Line1, Line2, Tag).

port_record_cover(Port, File, Line1, Line2, Tag) :-
    incr_record_cover(Port, File, Line1, Line2, Tag).

incr_record_cover(Port, File, Line1, Line2, Tag) :-
    ( retract(covered_db(File, Line1, Line2, Port, Tag, Count1))
    ->Count is Count1 + 1
      %succ(Count1, Count
    ; Count=1
    ),
    assertz(covered_db(File, Line1, Line2, Port, Tag, Count)).

decr_record_cover(Port, OutPort, File, Line1, Line2, Tag) :-
    ( retract(covered_db(File, Line1, Line2, Port, Tag, Count1))
    ->succ(Count, Count1),
      ( Count =:= 0
      ->true
      ; assertz(covered_db(File, Line1, Line2, Port, Tag, Count))
      ),
      OutPort = (success)
    ; OutPort = multi
    ).

reset_cover :- reset_cover(_).

reset_cover(Tag) :-
    retractall(covered_db(_, _, _, _, Tag, _)).
